// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.TypeProvider

namespace Quotations
type internal Helper =
    /// <summary>Makes a quotation of the form
    /// <code>
    ///   for v in s do
    ///     body</code></summary>
    static member MakeFor<'a,'b when 'a :> seq<'b>>(v, s, body) =
        let q = <@@ for el in (%%s : 'a) do
                        %%body @@>
        // replace 'el' variable with v, leaving everything else the same
        // we know that q is of the form 
        //     let inputSequence = s
        //     let enumerator = inputSequence.GetEnumerator()
        //     try
        //         while enumerator.MoveNext() do
        //             let el = enumerator.Current
        //             ...
        //     finally
        //         ...
        let (Quotations.Patterns.Let(v0,e0,Quotations.Patterns.Let(v1,e1,Quotations.Patterns.TryFinally(Quotations.Patterns.WhileLoop(e2, Quotations.Patterns.Let(v3,e3,b3)), e4)))) = q
        Quotations.Expr.Let(v0,e0,Quotations.Expr.Let(v1,e1,Quotations.Expr.TryFinally(Quotations.Expr.WhileLoop(e2, Quotations.Expr.Let(v,e3,b3)), e4)))
        

module Expr =
    /// <summary>Makes a quotation of the form
    /// <code>
    ///   for v in s do
    ///     body</code></summary>
    let internal For(v:Quotations.Var, s:Quotations.Expr, b:Quotations.Expr) =
        let seqTy = s.Type
        let eltTy = seqTy.GetInterface("System.Collections.Generic.IEnumerable`1").GetGenericArguments().[0]
        typeof<Helper>.GetMethod("MakeFor", enum 0xffffffff).MakeGenericMethod(seqTy, eltTy).Invoke(null, [|v;s;b|]) :?> Quotations.Expr
        
namespace Bond.TypeProvider.DesignTime

open System
open System.Reflection
open System.Collections.Generic
open System.IO
open ProviderImplementation.ProvidedTypes
open Microsoft.FSharp.Core.CompilerServices
open Bond
open Bond.Protocols
open Bond.IO.Unsafe
open Bond.TypeProvider

// Hard to create quotations calling methods with out arguments, so wrap instead
module RuntimeHelpers = 
    let ReadFieldBegin(reader : ITaggedProtocolReader) =
        reader.ReadFieldBegin()
        
    let ReadValueContainerBegin(reader : ITaggedProtocolReader) : _*_ =
        reader.ReadContainerBegin()
        
    let ReadKeyValueContainerBegin(reader : ITaggedProtocolReader) : _*_*_ =
        reader.ReadContainerBegin()

type private fieldInfo = { id : uint16; metadata : Metadata; defaultExpr : Quotations.Expr; defaultValue : obj; fieldType : TypeDef }

/// Convert SchemaDefs (and their components) to equivalent quotation literals
module private SchemaQuotation = 

    /// Given a function for converting list elements to quotations and a list of values, generate a quoted list 
    let rec private qList f = function
    | [] -> <@ [] @>
    | x::xs -> <@ %(f x) :: %(qList f xs) @>

    /// Given a dictionary, generate a quotation building an equivalent dictionary
    let private quoteDict d =
        <@ System.Collections.Generic.Dictionary(dict %(d |> List.ofSeq |> qList (fun (KeyValue(k,v)) -> <@ k, v @>))) @>

    /// Given a Variant, produce an equivalent quotation
    let quoteVariant (v:Variant) =
        if v.nothing then 
            <@ Variant(nothing = true) @>
        elif v.int_value <> 0L then
                let v = v.int_value in <@ Variant(int_value = v) @>
        elif v.uint_value <> 0uL then
                let v = v.uint_value in <@ Variant(uint_value = v) @>
        elif v.double_value <> 0.0 then
                let v = v.double_value in <@ Variant(double_value = v) @>
        elif v.string_value <> "" then
                let v = v.string_value in <@ Variant(string_value = v) @>
        elif v.wstring_value <> "" then
                let v = v.wstring_value in <@ Variant(wstring_value = v) @>
        else 
            <@ Variant() @>

    /// Given a Metadata, produce an equivalent quotation
    let quoteMetadata (m:Metadata) =
        let qn = m.qualified_name
        let nm = m.name
        let md = m.modifier
        <@ Metadata(qualified_name = qn, name = nm, modifier = md, default_value = %(quoteVariant m.default_value), attributes = %(quoteDict m.attributes)) @>


[<TypeProvider>]
type public BondTypeProvider(cfg:TypeProviderConfig) =
    inherit TypeProviderForNamespaces()

    // TODO: Is there a better way to enable quotation deserialization to find Bond.dll types?  This seems brittle.
    static let asms = [typeof<IProtocolWriter>.Assembly]
    static do System.AppDomain.CurrentDomain.add_AssemblyResolve(fun _ ev -> printfn "%s" ev.Name; match asms |> List.tryFind (fun asm -> asm.FullName = ev.Name) with | Some asm -> asm | None -> null)

    let memo f =
        let d = System.Collections.Generic.Dictionary(HashIdentity.Structural)
        fun x y ->
            if not <| d.ContainsKey(x,y) then
                d.[(x,y)] <- f x y
            d.[(x,y)]

    let runtimeAssembly = typeof<BondTypeProvider>.Assembly

    let ns = "Bond.TypeProvider"
    let schemaTy = ProvidedTypeDefinition(runtimeAssembly, ns, "SchemaTypeProvider", None)
    let filename = ProvidedStaticParameter("FilePath", typeof<string>)
    let protTy = ProvidedStaticParameter("Protocol", typeof<ProtocolType>, int ProtocolType.MARSHALED_PROTOCOL)
    let helpText = """<summary>Typed representation of a Bond schema</summary>
                      <param name='FilePath'>Bond SchemaDef location</param>
                      <param name='Protocol'>Schema serialization protocol (marshalled by default)</param>"""

    do schemaTy.AddXmlDoc helpText
    do schemaTy.DefineStaticParameters([filename; protTy;], memo (fun tyName [| :? string as filename; :? int as prot; |] ->
        
        let uri =
            if Uri.IsWellFormedUriString(filename, UriKind.Relative) then
                Uri(Path.Combine(Path.Combine(cfg.ResolutionFolder, filename)))
            else // note: this just works for full paths even without leading "file:///"
                System.Uri(filename)

        /// the SchemaDef that we will reflect as provided types
        let schemaContents =
            // Deserialize the SchemaDef from the Uri using the relevant ITaggedProtocolReader
            use strm = 
                if uri.IsFile then
                    File.OpenRead(uri.LocalPath) :> Stream
                else
                    let client = new System.Net.WebClient()
                    let content = client.DownloadData(uri)
                    new MemoryStream(content) :> _
            let def = 
                if enum prot = ProtocolType.MARSHALED_PROTOCOL then
                    Unmarshal<SchemaDef>.From(new InputStream(strm))
                else
                    let rdr : ITaggedProtocolReader = 
                        match enum prot with
                        | ProtocolType.COMPACT_PROTOCOL -> upcast new CompactBinaryReader<InputStream>(new InputStream(strm)) 
                        | ProtocolType.FAST_PROTOCOL -> upcast new FastBinaryReader<InputStream>(new InputStream(strm))
                        | p -> failwithf "Unrecognized protocol : %A" p
                    Deserialize<SchemaDef>.From(rdr)
            def

        /// maps struct IDs to provided type
        let provTys = System.Collections.Generic.Dictionary()
        
        /// maps struct i => Tuple<...>
        let tupTys = System.Collections.Generic.Dictionary()

        /// transitive closure of related struct indices
        let structsFrom (s:SchemaDef) idx =
            let rec structComponents (t:TypeDef) = 
                if t.id = BondDataType.BT_STRUCT then Set.singleton t.struct_def
                elif t.element = null then Set.empty
                else
                    match t.id with
                    | BondDataType.BT_LIST | BondDataType.BT_SET ->
                        structComponents t.element
                    | BondDataType.BT_MAP ->
                        Set.union (structComponents t.element) (structComponents t.key)
                    | BondDataType.BT_STRUCT ->
                        Set.singleton t.struct_def
                    | _ -> Set.empty
            let rec loop seen idx =
                let frontier = 
                    s.structs.[int idx].fields
                    |> Seq.map (fun f -> structComponents f.``type``)
                    |> Set.unionMany
                if Set.isSubset frontier seen then seen
                else 
                    frontier - seen
                    |> Set.fold loop (Set.union seen frontier)
            loop Set.empty idx        
        
        /// gets the representation type and full (unerased) type corresponding to the given TypeDef
        let rec typeForBondType (t:TypeDef) =
            match t.id with
            | BondDataType.BT_BOOL ->    typeof<bool>   , typeof<bool>      
            | BondDataType.BT_DOUBLE ->  typeof<float>  , typeof<float>     
            | BondDataType.BT_FLOAT ->   typeof<float32>, typeof<float32>   
            | BondDataType.BT_INT16 ->   typeof<int16>  , typeof<int16>     
            | BondDataType.BT_INT32 ->   typeof<int>    , typeof<int>       
            | BondDataType.BT_INT64 ->   typeof<int64>  , typeof<int64>     
            | BondDataType.BT_INT8  ->   typeof<sbyte>  , typeof<sbyte>     
            | BondDataType.BT_UINT16 ->  typeof<uint16> , typeof<uint16>   
            | BondDataType.BT_UINT32 ->  typeof<uint32> , typeof<uint32>    
            | BondDataType.BT_UINT64 ->  typeof<uint64> , typeof<uint64>    
            | BondDataType.BT_UINT8  ->  typeof<byte>   , typeof<byte>      
            | BondDataType.BT_STRING ->  typeof<string> , typeof<string>    
            | BondDataType.BT_WSTRING -> typeof<string> , typeof<string>  
            | BondDataType.BT_STRUCT -> typeof<obj>, provTys.[t.struct_def]
            | BondDataType.BT_LIST -> 
                let mkTy t = 
                    let tyDef = typedefof<_ list>
                    tyDef.MakeGenericType([|t|])
                let repr, full = typeForBondType t.element                                      
                mkTy repr, mkTy full
            | BondDataType.BT_MAP -> 
                let mkTy k v = 
                    let tyDef = typedefof<Map<_,_>>
                    tyDef.MakeGenericType([|k;v|])
                let krepr, kfull = typeForBondType t.key
                let vrepr, vfull = typeForBondType t.element
                mkTy krepr vrepr, mkTy kfull vfull
            | BondDataType.BT_SET -> 
                let mkTy t = 
                    let tyDef = typedefof<Set<_>>
                    tyDef.MakeGenericType([|t|])
                let repr, full = typeForBondType t.element
                mkTy repr, mkTy full
            | BondDataType.BT_STOP
            | BondDataType.BT_STOP_BASE
            | BondDataType.BT_UNAVAILABLE 
            | _ as ty -> failwith (sprintf "Unexpected BondDataType: %A" ty)

        /// Given a TypeDef, an expression for an ITaggedProtocolReader, and a dictionary mapping struct IDs to readers, produces an expression that reads the corresponding type of value from a reader
        // TODO: Use ReadHelper to make this more flexible (e.g. integral promotion)
        let rec readerForBondType (t:TypeDef) : Quotations.Expr<ITaggedProtocolReader> -> IDictionary<uint16,_> -> _ = 
            match t.id with
            | BondDataType.BT_BOOL ->    fun rdr _ -> <@@ (%rdr).ReadBool() @@>  
            | BondDataType.BT_DOUBLE ->  fun rdr _ -> <@@ (%rdr).ReadDouble() @@>
            | BondDataType.BT_FLOAT ->   fun rdr _ -> <@@ (%rdr).ReadFloat() @@> 
            | BondDataType.BT_INT16 ->   fun rdr _ -> <@@ (%rdr).ReadInt16() @@> 
            | BondDataType.BT_INT32 ->   fun rdr _ -> <@@ (%rdr).ReadInt32() @@> 
            | BondDataType.BT_INT64 ->   fun rdr _ -> <@@ (%rdr).ReadInt64() @@> 
            | BondDataType.BT_INT8  ->   fun rdr _ -> <@@ (%rdr).ReadInt8() @@> 
            | BondDataType.BT_UINT16 ->  fun rdr _ -> <@@ (%rdr).ReadUInt16() @@>
            | BondDataType.BT_UINT32 ->  fun rdr _ -> <@@ (%rdr).ReadUInt32() @@>
            | BondDataType.BT_UINT64 ->  fun rdr _ -> <@@ (%rdr).ReadUInt64() @@>
            | BondDataType.BT_UINT8  ->  fun rdr _ -> <@@ (%rdr).ReadUInt8() @@> 
            | BondDataType.BT_STRING ->  fun rdr _ -> <@@ (%rdr).ReadString() @@>
            | BondDataType.BT_WSTRING -> fun rdr _ -> <@@ (%rdr).ReadWString() @@>
            | BondDataType.BT_STRUCT ->  fun rdr d -> d.[t.struct_def] rdr
            | BondDataType.BT_LIST -> 
                    // produces a quotation like:
                    //    <@ let ct,_ = Helpers.ReadValueContainerBegin(rdr)
                    //       let arr = Array.zeroCreate<_> (int ct)
                    //       for i = 0 to arr.Length - 1 do
                    //           arr.[i] <- read rdr
                    //       rdr.ReadContainerEnd()
                    //       Array.toList arr @>
                    let elRep, _ = typeForBondType t.element
                    let arrRep = elRep.MakeArrayType()
                    let rdrGen = readerForBondType t.element
                    let zeroCreate = match <@ Array.zeroCreate 0 @> with | Quotations.Patterns.Call(None,zeroCreate,[_]) -> zeroCreate.GetGenericMethodDefinition().MakeGenericMethod(elRep)
                    let setArray = match <@ [| |].[0] <- 0 @> with | Quotations.Patterns.Call(None,setArray,[_;_;_]) -> setArray.GetGenericMethodDefinition().MakeGenericMethod(elRep)
                    let toList = match <@ Array.toList [| |] @> with | Quotations.Patterns.Call(None,toList,[_]) -> toList.GetGenericMethodDefinition().MakeGenericMethod(elRep)
                    fun rdr provFns ->
                        let read = rdrGen rdr provFns
                        let arr = Quotations.Var("arr", arrRep)
                        let ct = Quotations.Var("ct", typeof<int>)
                        let i = Quotations.Var("i", typeof<int>)
                        Quotations.Expr.Let(ct, Quotations.Expr.TupleGet(<@@ RuntimeHelpers.ReadValueContainerBegin %rdr @@>, 0), 
                            Quotations.Expr.Let(arr, Quotations.Expr.Call(zeroCreate, [ <@ int (%%(Quotations.Expr.Var ct) : int) @> ]),
                                Quotations.Expr.Sequential(
                                    Quotations.Expr.ForIntegerRangeLoop(i, <@ 0 @>, <@ %%Quotations.Expr.PropertyGet(Quotations.Expr.Var arr, arrRep.GetProperty("Length")) - 1 @>, 
                                        Quotations.Expr.Call(setArray, [Quotations.Expr.Var arr; Quotations.Expr.Var i; read])),
                                    Quotations.Expr.Sequential(
                                        <@@ (%rdr).ReadContainerEnd() @@>,
                                        Quotations.Expr.Call(toList, [Quotations.Expr.Var arr])))))
            | BondDataType.BT_SET ->
                    // produces a quotation like:
                    //    <@ let ct,_ = Helpers.ReadValueContainerBegin(rdr)
                    //       let arr = Array.zeroCreate<_> (int ct)
                    //       for i = 0 to arr.Length - 1 do
                    //           arr.[i] <- read rdr
                    //       rdr.ReadContainerEnd()
                    //       Set.ofArray arr @>
                    let elRep, _ = typeForBondType t.element
                    let arrRep = elRep.MakeArrayType()
                    let rdrGen = readerForBondType t.element
                    let zeroCreate = match <@ Array.zeroCreate 0 @> with | Quotations.Patterns.Call(None,zeroCreate,[_]) -> zeroCreate.GetGenericMethodDefinition().MakeGenericMethod(elRep)
                    let setArray = match <@ [| |].[0] <- 0 @> with | Quotations.Patterns.Call(None,setArray,[_;_;_]) -> setArray.GetGenericMethodDefinition().MakeGenericMethod(elRep)
                    let ofArray = match <@ Set.ofArray [| |] @> with | Quotations.Patterns.Call(None,ofArray,[_]) -> ofArray.GetGenericMethodDefinition().MakeGenericMethod(elRep)
                    fun rdr provFns ->
                        let read = rdrGen rdr provFns
                        let arr = Quotations.Var("arr", arrRep)
                        let ct = Quotations.Var("ct", typeof<int>)
                        let i = Quotations.Var("i", typeof<int>)
                        Quotations.Expr.Let(ct, Quotations.Expr.TupleGet(<@@ RuntimeHelpers.ReadValueContainerBegin %rdr @@>, 0), 
                            Quotations.Expr.Let(arr, Quotations.Expr.Call(zeroCreate, [ <@ int (%%(Quotations.Expr.Var ct) : int) @> ]),
                                Quotations.Expr.Sequential(
                                    Quotations.Expr.ForIntegerRangeLoop(i, <@ 0 @>, <@ %%Quotations.Expr.PropertyGet(Quotations.Expr.Var arr, arrRep.GetProperty("Length")) - 1 @>, 
                                        Quotations.Expr.Call(setArray, [Quotations.Expr.Var arr; Quotations.Expr.Var i; read])),
                                    Quotations.Expr.Sequential(
                                        <@@ (%rdr).ReadContainerEnd() @@>,
                                        Quotations.Expr.Call(ofArray, [Quotations.Expr.Var arr])))))

            | BondDataType.BT_MAP -> 
                    // produces a quotation like:
                    //    <@ let ct,_,_ = Helpers.ReadKeyValueContainerBegin(rdr)
                    //       let arr = Array.zeroCreate<_*_> (int ct)
                    //       for i = 0 to arr.Length - 1 do
                    //           arr.[i] <- readKey rdr, readVal rdr
                    //       rdr.ReadContainerEnd()
                    //       Map.ofArray arr @>
                    let elRep, _ = typeForBondType t.element
                    let keyRep, _ = typeForBondType t.key
                    let tupRep = Reflection.FSharpType.MakeTupleType([|keyRep; elRep|])
                    let arrRep = tupRep.MakeArrayType()
                    let elRdrGen = readerForBondType t.element
                    let keyRdrGen = readerForBondType t.key
                    let zeroCreate = match <@ Array.zeroCreate 0 @> with | Quotations.Patterns.Call(None,zeroCreate,[_]) -> zeroCreate.GetGenericMethodDefinition().MakeGenericMethod(tupRep)
                    let setArray = match <@ [| |].[0] <- 0 @> with | Quotations.Patterns.Call(None,setArray,[_;_;_]) -> setArray.GetGenericMethodDefinition().MakeGenericMethod(tupRep)
                    let ofArray = match <@ Map.ofArray [| |] @> with | Quotations.Patterns.Call(None,ofArray,[_]) -> ofArray.GetGenericMethodDefinition().MakeGenericMethod(keyRep, elRep)
                    fun rdr provFns ->
                        let readKey = keyRdrGen rdr provFns
                        let readEl = elRdrGen rdr provFns
                        let arr = Quotations.Var("arr", arrRep)
                        let ct = Quotations.Var("ct", typeof<int>)
                        let i = Quotations.Var("i", typeof<int>)
                        Quotations.Expr.Let(ct, Quotations.Expr.TupleGet(<@@ RuntimeHelpers.ReadKeyValueContainerBegin %rdr @@>, 0), 
                            Quotations.Expr.Let(arr, Quotations.Expr.Call(zeroCreate, [ <@ int (%%(Quotations.Expr.Var ct) : int) @> ]),
                                Quotations.Expr.Sequential(
                                    Quotations.Expr.ForIntegerRangeLoop(i, <@ 0 @>, <@ %%Quotations.Expr.PropertyGet(Quotations.Expr.Var arr, arrRep.GetProperty("Length")) - 1 @>, 
                                        Quotations.Expr.Call(setArray, [Quotations.Expr.Var arr; Quotations.Expr.Var i; Quotations.Expr.NewTuple [readKey; readEl]])),
                                    Quotations.Expr.Sequential(
                                        <@@ (%rdr).ReadContainerEnd() @@>,
                                        Quotations.Expr.Call(ofArray, [Quotations.Expr.Var arr])))))

            | BondDataType.BT_STOP
            | BondDataType.BT_STOP_BASE
            | BondDataType.BT_UNAVAILABLE 
            | _ as ty -> failwith (sprintf "Unexpected BondDataType: %A" ty)

        /// Given a TypeDef, an expression representing an IProtocolWriter, and an expression representing the value to write, produces an expression for writing the value
        let rec writerForBondType (t:TypeDef) : Quotations.Expr<IProtocolWriter> -> _ -> IDictionary<uint16,_> -> _ =
            match t.id with
            | BondDataType.BT_BOOL ->    fun wrtr e _ -> <@@ (%wrtr).WriteBool(%%e) @@>
            | BondDataType.BT_DOUBLE ->  fun wrtr e _ -> <@@ (%wrtr).WriteDouble(%%e) @@>
            | BondDataType.BT_FLOAT ->   fun wrtr e _ -> <@@ (%wrtr).WriteFloat(%%e) @@>
            | BondDataType.BT_INT16 ->   fun wrtr e _ -> <@@ (%wrtr).WriteInt16(%%e) @@>
            | BondDataType.BT_INT32 ->   fun wrtr e _ -> <@@ (%wrtr).WriteInt32(%%e) @@>
            | BondDataType.BT_INT64 ->   fun wrtr e _ -> <@@ (%wrtr).WriteInt64(%%e) @@>
            | BondDataType.BT_INT8  ->   fun wrtr e _ -> <@@ (%wrtr).WriteInt8(%%e) @@>
            | BondDataType.BT_UINT16 ->  fun wrtr e _ -> <@@ (%wrtr).WriteUInt16(%%e) @@>
            | BondDataType.BT_UINT32 ->  fun wrtr e _ -> <@@ (%wrtr).WriteUInt32(%%e) @@>
            | BondDataType.BT_UINT64 ->  fun wrtr e _ -> <@@ (%wrtr).WriteUInt64(%%e) @@>
            | BondDataType.BT_UINT8  ->  fun wrtr e _ -> <@@ (%wrtr).WriteUInt8(%%e) @@>
            | BondDataType.BT_STRING ->  fun wrtr e _ -> <@@ (%wrtr).WriteString(%%e) @@>
            | BondDataType.BT_WSTRING -> fun wrtr e _ -> <@@ (%wrtr).WriteWString(%%e) @@>
            | BondDataType.BT_STRUCT ->  fun wrtr e d -> d.[t.struct_def] wrtr e 
            | BondDataType.BT_LIST -> 
                // produces a quotation like:
                //    <@ wrtr.WriteContainerBegin(l.Count, dataType)
                //       for e in l do
                //           write wrtr e
                //       wrtr.WriteContainerEnd() @>
                let lstRep, _ = typeForBondType t
                let elRep, _ = typeForBondType t.element
                let wrtrGen = writerForBondType t.element
                let lenProp = lstRep.GetProperty("Length")
                fun wrtr lstExpr d ->
                    let write e = wrtrGen wrtr e d
                    Quotations.Expr.Sequential(
                        let dataType = t.element.id
                        <@@ (%wrtr).WriteContainerBegin((%%Quotations.Expr.PropertyGet(lstExpr, lenProp) : int), dataType) @@>,
                        Quotations.Expr.Sequential(
                            let e = Quotations.Var("e", elRep)
                            Quotations.Expr.For(e, lstExpr, write (Quotations.Expr.Var e)),
                            <@@ (%wrtr).WriteContainerEnd() @@>))                            
            | BondDataType.BT_SET ->                 
                // produces a quotation like:
                //    <@ wrtr.WriteContainerBegin(s.Count, dataType)
                //       for e in s do
                //           write wrtr e
                //       wrtr.WriteContainerEnd() @>
                let setRep, _ = typeForBondType t
                let elRep, _ = typeForBondType t.element
                let wrtrGen = writerForBondType t.element
                let lenProp = setRep.GetProperty("Count")
                fun wrtr setExpr d ->
                    let write e = wrtrGen wrtr e d
                    Quotations.Expr.Sequential(
                        let dataType = t.element.id
                        <@@ (%wrtr).WriteContainerBegin((%%Quotations.Expr.PropertyGet(setExpr, lenProp) : int), dataType) @@>,
                        Quotations.Expr.Sequential(
                            let e = Quotations.Var("e", elRep)
                            Quotations.Expr.For(e, setExpr, write (Quotations.Expr.Var e)),
                            <@@ (%wrtr).WriteContainerEnd() @@>))                            

            | BondDataType.BT_MAP -> 
                // produces a quotation like:
                //    <@ wrtr.WriteContainerBegin(s.Count, keyDataType, elDataType)
                //       for e in s do
                //           write wrtr e
                //       wrtr.WriteContainerEnd() @>
                let dictRep, _ = typeForBondType t
                let elRep, _ = typeForBondType t.element
                let keyRep, _ = typeForBondType t.key
                let elWrtrGen = writerForBondType t.element
                let keyWrtrGen = writerForBondType t.key
                let lenProp = dictRep.GetProperty("Count")
                fun wrtr dictExpr d ->
                    let writeKey e = keyWrtrGen wrtr e d
                    let writeEl e = elWrtrGen wrtr e d
                    Quotations.Expr.Sequential(
                        let elType = t.element.id
                        let keyType = t.key.id
                        <@@ (%wrtr).WriteContainerBegin((%%Quotations.Expr.PropertyGet(dictExpr, lenProp) : int), keyType, elType) @@>,
                        Quotations.Expr.Sequential(
                            let itemType = typedefof<KeyValuePair<_,_>>.MakeGenericType(keyRep, elRep)
                            let kvp = Quotations.Var("kvp", itemType)
                            Quotations.Expr.For(kvp, dictExpr, Quotations.Expr.Sequential(writeKey (Quotations.Expr.PropertyGet(Quotations.Expr.Var kvp, itemType.GetProperty("Key"))), writeEl (Quotations.Expr.PropertyGet(Quotations.Expr.Var kvp, itemType.GetProperty("Value"))))),
                            <@@ (%wrtr).WriteContainerEnd() @@>))                            

            | BondDataType.BT_STOP
            | BondDataType.BT_STOP_BASE
            | BondDataType.BT_UNAVAILABLE 
            | _ as ty -> failwith (sprintf "Unexpected BondDataType: %A" ty)

        let rootStruct = schemaContents.structs.[int schemaContents.root.struct_def]

        let containerTy = ProvidedTypeDefinition(runtimeAssembly, ns, tyName, None)

        let bondTyIsContainer = function
        | BondDataType.BT_LIST | BondDataType.BT_SET | BondDataType.BT_MAP -> true
        | _ -> false

        let rec defaultExpr (t:TypeDef) (v:Variant) = 
            match t.id with
            | BondDataType.BT_BOOL ->   let b = v.uint_value <> 0UL       in <@@ b @@> 
            | BondDataType.BT_DOUBLE -> let f = v.double_value            in <@@ f @@> 
            | BondDataType.BT_FLOAT ->  let f = v.double_value |> float32 in <@@ f @@>
            | BondDataType.BT_INT16 ->  let i = v.int_value |> int16      in <@@ i @@>
            | BondDataType.BT_INT32 ->  let i = v.int_value |> int        in <@@ i @@>
            | BondDataType.BT_INT64 ->  let i = v.int_value               in <@@ i @@>
            | BondDataType.BT_INT8  ->  let i = v.int_value |> sbyte      in <@@ i @@>
            | BondDataType.BT_UINT16 -> let i = v.uint_value |> uint16    in <@@ i @@>
            | BondDataType.BT_UINT32 -> let i = v.uint_value |> uint32    in <@@ i @@>
            | BondDataType.BT_UINT64 -> let i = v.uint_value |> uint64    in <@@ i @@>
            | BondDataType.BT_UINT8  -> let i = v.uint_value |> byte      in <@@ i @@>
            | BondDataType.BT_STRING -> let s = v.string_value            in <@@ s @@>
            | BondDataType.BT_WSTRING -> let s = v.wstring_value          in <@@ s @@>
            | BondDataType.BT_STRUCT -> <@@ null : obj @@>
            | BondDataType.BT_LIST -> 
                let (ty,_) = typeForBondType t
                Quotations.Expr.NewUnionCase(Reflection.FSharpType.GetUnionCases ty |> Array.find (fun uc -> uc.Name = "Empty"), [])
            | BondDataType.BT_SET ->
                let (elTy,_) = typeForBondType t.element
                let (Quotations.Patterns.Call(None,emptyMethod,[])) = <@ Set.empty @>
                Quotations.Expr.Call(emptyMethod.GetGenericMethodDefinition().MakeGenericMethod(elTy), [])
            | BondDataType.BT_MAP ->
                let (keyTy,_) = typeForBondType t.key
                let (elTy,_) = typeForBondType t.element
                let (Quotations.Patterns.Call(None,emptyMethod,[])) = <@ Map.empty @>
                Quotations.Expr.Call(emptyMethod.GetGenericMethodDefinition().MakeGenericMethod(keyTy,elTy), [])
            | BondDataType.BT_STOP
            | BondDataType.BT_STOP_BASE
            | BondDataType.BT_UNAVAILABLE 
            | _ as ty -> failwith (sprintf "Unexpected BondDataType: %A" ty)

        let rec defaultValue (t:TypeDef) (v:Variant) = 
            match t.id with
            | BondDataType.BT_BOOL ->   v.uint_value <> 0UL       |> box
            | BondDataType.BT_DOUBLE -> v.double_value            |> box 
            | BondDataType.BT_FLOAT ->  v.double_value |> float32 |> box
            | BondDataType.BT_INT16 ->  v.int_value |> int16      |> box
            | BondDataType.BT_INT32 ->  v.int_value |> int        |> box
            | BondDataType.BT_INT64 ->  v.int_value               |> box
            | BondDataType.BT_INT8  ->  v.int_value |> sbyte      |> box
            | BondDataType.BT_UINT16 -> v.uint_value |> uint16    |> box
            | BondDataType.BT_UINT32 -> v.uint_value |> uint32    |> box
            | BondDataType.BT_UINT64 -> v.uint_value |> uint64    |> box
            | BondDataType.BT_UINT8  -> v.uint_value |> byte      |> box
            | BondDataType.BT_STRING -> v.string_value            |> box
            | BondDataType.BT_WSTRING -> v.wstring_value          |> box
            | BondDataType.BT_STRUCT 
            | BondDataType.BT_LIST 
            | BondDataType.BT_SET 
            | BondDataType.BT_MAP    -> null
            | BondDataType.BT_STOP
            | BondDataType.BT_STOP_BASE
            | BondDataType.BT_UNAVAILABLE 
            | _ as ty -> failwith (sprintf "Unexpected BondDataType: %A" ty)

        /// Gets the list of (field ID, metadata, default value (expression), field type) for each field in the nth type
        let fieldsFor i = 
            [for f in schemaContents.structs.[i].fields ->
                { id = f.id; metadata = f.metadata; defaultExpr = defaultExpr f.``type`` f.metadata.default_value; defaultValue = defaultValue f.``type`` f.metadata.default_value; fieldType = f.``type``}]
            |> List.sortBy (fun fi -> fi.id)

        containerTy.AddMembers(
            schemaContents.structs 
            |> Seq.toList
            |> List.mapi (fun i st -> (i,st))
            |> List.filter (fun (_,st) -> st.base_def = null) // we don't currently support inheritance
            |> List.map (fun (i,st) ->

                let reprTy = 
                    lazy 
                        fieldsFor i |> List.map (fun fieldInfo -> typeForBondType fieldInfo.fieldType |> fst)
                        |> Array.ofList
                        |> Reflection.FSharpType.MakeTupleType

                let stTy = ProvidedTypeDefinition(st.metadata.name, Some(typeof<obj>))
                
                provTys.[uint16 i] <- stTy
                tupTys.[uint16 i] <- lazy reprTy.Value

                stTy.AddMembersDelayed(fun () -> 

                    let props = 
                        fieldsFor i |> List.mapi (fun idx fieldInfo -> 
                            let (_,ty) = typeForBondType fieldInfo.fieldType
                            ProvidedProperty(fieldInfo.metadata.name, ty, 
                                GetterCode = fun [this] -> Quotations.Expr.TupleGet(Quotations.Expr.Coerce(this, tupTys.[uint16 i].Value), idx)) :> MemberInfo)
                                                           
                    let unitVal = Quotations.Expr.Value(null, typeof<unit>)

                    let rec mkFnTy (dom::tys) = 
                        let rng = 
                            match tys with
                            | [rng] -> rng
                            | l -> mkFnTy l
                        Reflection.FSharpType.MakeFunctionType(dom, rng)

                    let relatedStructs = 
                        structsFrom schemaContents (uint16 i)
                        |> Set.add (uint16 i)

                    let readVars = 
                        relatedStructs 
                        |> Seq.map (fun i -> i, Quotations.Var(sprintf "read%i" i, mkFnTy [typeof<ITaggedProtocolReader>; tupTys.[i].Value]))
                        |> dict

                    let writeVars =
                        relatedStructs 
                        |> Seq.map (fun i -> i, Quotations.Var(sprintf "write%i" i, mkFnTy [typeof<IProtocolWriter>; tupTys.[i].Value; typeof<unit>]))
                        |> dict

                    let allSequential = function 
                        | [] -> <@@ () @@>
                        | xs -> List.reduce (fun e1 e2 -> Quotations.Expr.Sequential(e1, e2)) xs

                    let writeDefns =
                        [for (KeyValue(idx,wrVar)) in writeVars -> 
                            let writeVarExprs = [for i in relatedStructs -> 
                                                    i, 
                                                    fun wrtr e -> 
                                                        Quotations.Expr.Application(Quotations.Expr.Application(Quotations.Expr.Var writeVars.[i], wrtr), Quotations.Expr.Coerce(e, tupTys.[i].Value))] |> dict
//                          let write (ipw : IProtocolWriter) = 
//                              let writeAllFields = not ipw.MayOmitFields
//                              ipw.WriteStructBegin(structMeta)
//
//                              if writeAllFields || tupGet 1 <> def then
//                                  ipw.WriteFieldBegin(BondDataType.BT_INT32, 1, valueMeta)
//                                  ipw.WriteInt32(tupGet n)
//                                  ipw.WriteFieldEnd()
//                              else
//                                  ipw.WriteFieldOmitted(BondDataType.BT_INT32, 1, valueMeta)
//
//                              if writeAllFields || (tupGet 2).Count <> 0 then
//                                  ipw.WriteFieldBegin(BondDataType.BT_LIST, 2, childrenMeta)
//                                  // loop
//                                  ipw.WriteFieldEnd()
//                              else
//                                  ipw.WriteFieldOmitted(BondDataType.BT_INT32, 2, childrenMeta)
//
//                              ipw.WriteStructEnd(false)
//                          let pass0 = wrtr.GetPass0Writer()
//                          if pass0 <> null then 
//                              write pass0
//                              write wrtr
//                              wrtr.EndDoublePass()
//                          else
//                              write wrtr


                            let writer = Quotations.Var("wrtr", typeof<IProtocolWriter>)
                            let value = Quotations.Var("value", tupTys.[idx].Value)
                            let expr = 
                                Quotations.Expr.Lambda(writer, 
                                    Quotations.Expr.Lambda(value, 
                                        let writer = Quotations.Expr.Cast<IProtocolWriter>(Quotations.Expr.Var writer)
                                        let write = Quotations.Var("write", typeof<IProtocolWriter -> unit>)
                                        Quotations.Expr.Let(
                                            write, 
                                            (let ipw = Quotations.Var("ipw", typeof<IProtocolWriter>)
                                             Quotations.Expr.Lambda(ipw,
                                                let ipw = Quotations.Expr.Cast<IProtocolWriter>(Quotations.Expr.Var ipw)
                                                // TODO: writeAllFields not needed 
                                                let writeAllFields = Quotations.Var("writeAllFields", typeof<bool>)
                                                Quotations.Expr.Let(writeAllFields, <@ true @>,
                                                    Quotations.Expr.Sequential(
                                                        let writeBegin = <@ (%ipw).WriteStructBegin((%SchemaQuotation.quoteMetadata schemaContents.structs.[int idx].metadata)) @>
                                                        let writeFields =
                                                            fieldsFor (int idx)
                                                            |> List.mapi (fun idx fieldInfo -> 
                                                                            let id = fieldInfo.id
                                                                            let elt = Quotations.Expr.TupleGet(Quotations.Expr.Var value, idx)
                                                                            let cond =  // val <> default  (or val.Count <> 0)
                                                                                if not (bondTyIsContainer fieldInfo.fieldType.id) then
                                                                                    let (Quotations.Patterns.Call(_,neq,[_;_])) = <@ 1 <> 2 @>
                                                                                    Quotations.Expr.Call(neq.GetGenericMethodDefinition().MakeGenericMethod(fieldInfo.defaultExpr.Type), [elt; fieldInfo.defaultExpr])
                                                                                else 
                                                                                    // not {List,Set,Map}.isEmpty
                                                                                    let (Quotations.Patterns.Call(None,m,[_])) =
                                                                                        match fieldInfo.fieldType.id with
                                                                                        | BondDataType.BT_LIST -> <@ List.isEmpty [] @>
                                                                                        | BondDataType.BT_SET -> <@ Set.isEmpty Set.empty @>
                                                                                        | BondDataType.BT_MAP -> <@ Map.isEmpty Map.empty @>
                                                                                    <@@ not (%%Quotations.Expr.Call(m.GetGenericMethodDefinition().MakeGenericMethod(elt.Type.GetGenericArguments()), [elt])) @@>
                                                                                                
                                                                            let bondTy = fieldInfo.fieldType.id
                                                                            let writeField = 
                                                                                <@ (%ipw).WriteFieldBegin(bondTy, id, %SchemaQuotation.quoteMetadata fieldInfo.metadata)
                                                                                   (%%writerForBondType fieldInfo.fieldType ipw elt writeVarExprs)
                                                                                   (%ipw).WriteFieldEnd() @>
                                                                            if fieldInfo.metadata.modifier <> Modifier.Optional then
                                                                                // if the field is required, always write it
                                                                                writeField
                                                                            else
                                                                                // otherwise, perform (writeAllFields + default) check to see if we must write it
                                                                                <@ if %%Quotations.Expr.Var writeAllFields || %%cond then
                                                                                        %writeField
                                                                                    else
                                                                                        (%ipw).WriteFieldOmitted(bondTy, id, %SchemaQuotation.quoteMetadata fieldInfo.metadata) @>)
                                                        writeBegin :: writeFields |> List.reduce (fun q1 q2 -> <@ %q1; %q2 @>), 
                                                        <@ (%ipw).WriteStructEnd() @>)))),
                                                                                                           
                                            let write = Quotations.Expr.Cast<IProtocolWriter->unit>(Quotations.Expr.Var write)
                                            <@ (%write) %writer @>)))                                                                    
                            wrVar, expr]

                    let NewTuple_ (expr : Quotations.Expr list) =
                        // BUGBUG: Quotations.Expr.NewTuple does not create a Tuple when called with one argument, this appear to be F# compiler bug 
                        let tupTy = [| for e in expr -> e.Type |] 
                                    |> Reflection.FSharpType.MakeTupleType 
                        if expr.Length > 1 then
                            Quotations.Expr.NewTuple(expr)
                        else
                            Quotations.Expr.NewObject(tupTy.GetConstructors().[0], expr)

                    let readDefns =
                        [for (KeyValue(idx,rdVar)) in readVars ->
                            let makeVarExprs = [for i in relatedStructs -> 
                                                    i, 
                                                    fun rdr -> 
                                                        Quotations.Expr.Application(Quotations.Expr.Var readVars.[i], rdr)] |> dict

//                          ipr.ReadStructBegin()
//                          if ipr.HasCapability(Protocol.Tagged) then
//                              let rec loop() =
//                                  let ty,id = Helpers.ReadFieldBegin ipr
//                                  if ty <> BondDataType.BT_STOP && ty <> BondDataType.BT_STOP_BASE then
//                                      if id = 1us then
//                                          readValue()
//                                      elif id = 2us then
//                                          readChildren()
//                                      else
//                                          ipr.Skip(ty)
//                                      ipr.ReadFieldEnd()
//                                      loop()
//                                  else printfn "Stopped with: %A" ty
//                              loop()
//                          else
//                              let canOmit = ipr.HasCapability(Protocol.CanOmitFields)
//                              if not (canOmit || ipr.ReadFieldOmitted()) then
//                                  readValue()
//                              if not (canOmit || ipr.ReadFieldOmitted()) then
//                                  readChildren()
//                          ipr.ReadStructEnd()


// TODO: need to handle required fields: 
//          add throwing "else" blocks to "if not ..."
//          add bitarray writing within loop and checking after

                            let reader = Quotations.Var("rdr", typeof<ITaggedProtocolReader>)

                            let fieldVarsAndVals = 
                                fieldsFor (int idx) 
                                |> List.map (fun fieldInfo ->
                                    let e = fieldInfo.defaultExpr
                                    let refTy = typedefof<_ ref>.MakeGenericType(e.Type)
                                    let (Quotations.Patterns.Call(None,refGet,[_])) = <@ !(ref 0) @>
                                    let var = Quotations.Var(fieldInfo.metadata.name, refTy)
                                    var, Quotations.Expr.NewRecord(refTy, [e]), Quotations.Expr.Call(refGet.GetGenericMethodDefinition().MakeGenericMethod(e.Type), [Quotations.Expr.Var var]))
                                |> List.toArray

                            let expr = 
                                let reader = Quotations.Expr.Cast<ITaggedProtocolReader>(Quotations.Expr.Var reader)

                                let readFieldFns =
                                    fieldsFor (int idx)
                                    |> List.mapi (fun fldIdx fieldInfo ->
                                        let mkRdr = readerForBondType fieldInfo.fieldType
                                        let fn = Quotations.Var(sprintf "read_%s" fieldInfo.metadata.name, typeof<unit->unit>)
                                        let read = mkRdr reader makeVarExprs
                                        fn, Quotations.Expr.Lambda(Quotations.Var("_", typeof<unit>), 
                                                let (Quotations.Patterns.Call(None,refSet,[_;_])) = <@ ref 0 := 0 @>
                                                let (var,_,_) = fieldVarsAndVals.[fldIdx]
                                                Quotations.Expr.Call(refSet.GetGenericMethodDefinition().MakeGenericMethod(fst (typeForBondType fieldInfo.fieldType)), [Quotations.Expr.Var var; read])))
                                    |> List.toArray
                                let fieldSwitch =
                                    let tyVar = Quotations.Var("ty", typeof<BondDataType>)
                                    let idVar = Quotations.Var("id", typeof<uint16>)
                                    Quotations.Expr.Lambda(tyVar, 
                                        Quotations.Expr.Lambda(idVar, 
                                            let fn = 
                                                fieldsFor (int idx)
                                                |> List.mapi (fun fldIdx fieldInfo ->
                                                    fun ty id' next ->
                                                        let id = fieldInfo.id
                                                        // TODO: throw exception if expected and actual type differ?
                                                        Quotations.Expr.IfThenElse(<@ %id' = id @>, <@ (%%Quotations.Expr.Var (fst readFieldFns.[fldIdx])) () : unit @>, next))
                                                |> List.fold (fun e f ty id -> f ty id (e ty id)) (fun ty _ -> <@@ (%reader).Skip(%ty) @@>)
                                            fn (tyVar |> Quotations.Expr.Var |> Quotations.Expr.Cast) (idVar |> Quotations.Expr.Var |> Quotations.Expr.Cast)))
                                
                                // TODO: read untagged
                                //let readFieldsInOrder =
                                //    [for (v,_) in readFieldFns ->
                                //        <@@ (%%Quotations.Expr.Var v) ()  @@>]
                                //    |> allSequential
                                    
                                let body = 
                                    <@@ (%reader).ReadStructBegin()
                                        let rec loop() =
                                            let ty,id = RuntimeHelpers.ReadFieldBegin %reader
                                            if ty <> BondDataType.BT_STOP && ty <> BondDataType.BT_STOP_BASE then
                                                (%%fieldSwitch) ty id
                                                (%reader).ReadFieldEnd()
                                                loop()
                                        loop()
                                        (%reader).ReadStructEnd() @@>

                                // inline fieldswitch instead of having it be a function
                                let simplify e = 
                                    let rec simplify = function
                                    | Quotations.Patterns.Application(Quotations.Patterns.Lambda(v,e), Quotations.Patterns.Var v') ->
                                        true, e.Substitute(fun v'' -> if v'' = v then Some(Quotations.Expr.Var v') else None) |> simplify |> snd
                                    | Quotations.Patterns.Application(f,b) as e ->
                                        let sf, ef = simplify f
                                        let sb, eb = simplify b 
                                        if sf || sb then true, Quotations.Expr.Application(ef, eb) |> simplify |> snd
                                        else false, e
                                    | Quotations.ExprShape.ShapeLambda(v,b) as e ->
                                        let sb, eb = simplify b
                                        sb, if sb then Quotations.Expr.Lambda(v, eb) else e
                                    | Quotations.ExprShape.ShapeCombination(o, l) as e ->
                                        let l' = List.map simplify l
                                        if List.exists fst l' then true, Quotations.ExprShape.RebuildShapeCombination(o, List.map snd l') |> simplify |> snd
                                        else false, e
                                    | Quotations.ExprShape.ShapeVar _ as e -> false, e
                                    simplify e |> snd

                                readFieldFns
                                |> Array.fold (fun b (v,e) -> Quotations.Expr.Let(v,e,b)) body
                                |> simplify

                            let expr = 
                                Quotations.Expr.Lambda(reader, 
                                    fieldVarsAndVals
                                    |> Array.fold (fun e (var,def,_) -> Quotations.Expr.Let(var, def, e)) (Quotations.Expr.Sequential(expr, NewTuple_(fieldVarsAndVals |> Array.map (fun (_,_,getVal) -> getVal) |> List.ofArray))))

                            rdVar, expr]
                    
                    let createInstance args =
                        List.zip args (fieldsFor i)
                        |> List.map (fun (arg:Quotations.Expr, f) -> 
                            if f.defaultValue = null then
                                // <@ if (arg :> obj) = null then defaultExpr else arg @>
                                // Note that we can't use the generic equality test at the actual arg type or it will throw a null reference exception, thanks to F#'s non-nullable type checking
                                Quotations.Expr.IfThenElse(<@ %%Quotations.Expr.Coerce(arg, typeof<obj>) = null @>, f.defaultExpr, arg)
                            else
                                arg)
                        |> NewTuple_
                    props @ [ProvidedConstructor(
                                [for (:? PropertyInfo as pi), fi in Seq.zip props (fieldsFor i) -> ProvidedParameter(pi.Name, pi.PropertyType, optionalValue = fi.defaultValue)], 
                                InvokeCode = createInstance)
                             ProvidedMethod(
                                "DeserializeFrom",
                                [ProvidedParameter("reader", typeof<ITaggedProtocolReader>)], 
                                stTy, 
                                IsStaticMethod = true, 
                                InvokeCode = fun [rdr] -> 
                                    Quotations.Expr.LetRecursive(
                                        readDefns, 
                                        Quotations.Expr.Application(Quotations.Expr.Var readVars.[uint16 i], rdr)))
                             ProvidedMethod(
                                "SerializeTo",
                                [ProvidedParameter("writer", typeof<IProtocolWriter>)], 
                                typeof<unit>, 
                                InvokeCode = fun [this;wrtr] -> 
                                    Quotations.Expr.LetRecursive(
                                        writeDefns, 
                                        Quotations.Expr.Application(
                                            Quotations.Expr.Application(Quotations.Expr.Var writeVars.[uint16 i], wrtr), 
                                            Quotations.Expr.Coerce(this, tupTys.[uint16 i].Value))))])
                stTy))
        
        containerTy))
    do base.AddNamespace(ns, [schemaTy])

module UnitTest

open System
open System.Collections.Generic 
open Microsoft.VisualStudio.TestTools.UnitTesting
open Bond
open Bond.Protocols
open Bond.IO
open Bond.IO.Unsafe
open Bond.TypeProvider

// Use type provider to create types from a file with marshaled runtime schema
type Ty = SchemaTypeProvider< @"unittest.schema.All">

let mkWriter strm : _ -> IProtocolWriter = function
| ProtocolType.SIMPLE_PROTOCOL -> upcast new SimpleBinaryWriter<IOutputStream>(strm)
| ProtocolType.COMPACT_PROTOCOL -> upcast new CompactBinaryWriter<IOutputStream>(strm)
| ProtocolType.FAST_PROTOCOL -> upcast new FastBinaryWriter<IOutputStream>(strm)
| p -> failwithf "Unsupported protocol: %A" p


let mkTaggedReader (data:ArraySegment<byte>) : _ -> ITaggedProtocolReader = function
| ProtocolType.COMPACT_PROTOCOL -> upcast new CompactBinaryReader<InputBuffer>(new InputBuffer(data))
| ProtocolType.FAST_PROTOCOL -> upcast new FastBinaryReader<InputBuffer>(new InputBuffer(data))
| p -> failwithf "Unsupported tagged protocol: %A" p


let mkUntaggedReader (data:ArraySegment<byte>) : _ -> IUntaggedProtocolReader = function
| ProtocolType.SIMPLE_PROTOCOL -> upcast new SimpleBinaryReader<InputBuffer>(new InputBuffer(data))
| p -> failwithf "Unsupported untagged protocol: %A" p


let inline serialize p obj =
    let output = new OutputBuffer()
    let writer = mkWriter output p
    (^t : (member SerializeTo : IProtocolWriter -> unit)(obj, writer))
    output.Data

let inline deserialize p (data:ArraySegment<byte>) =
    let reader = mkTaggedReader data p
    (^t : (static member DeserializeFrom : ITaggedProtocolReader -> ^t) reader)

let inline deserializeUntagged p (data:ArraySegment<byte>) =
    let reader = mkUntaggedReader data p
    (^t : (static member DeserializeFrom : IUntaggedProtocolReader -> ^t) reader)

let inline serializeCB obj =
    serialize ProtocolType.COMPACT_PROTOCOL obj

let inline deserializeCB (data:ArraySegment<byte>) = 
    deserialize ProtocolType.COMPACT_PROTOCOL data

let inline serializeFB obj =
    serialize ProtocolType.FAST_PROTOCOL obj

let inline deserializeFB (data:ArraySegment<byte>) = 
    deserialize ProtocolType.FAST_PROTOCOL data

let inline serializeSP obj =
    serialize ProtocolType.SIMPLE_PROTOCOL obj

let inline deserializeSP (data:ArraySegment<byte>) = 
    deserializeUntagged ProtocolType.SIMPLE_PROTOCOL data

let inline roundtripCB src =
    serializeCB src |> deserializeCB

let inline roundtripFB src =
    serializeFB src |> deserializeFB

let inline roundtripSP src =
    serializeSP src |> deserializeSP

let inline testRoundtripCB (src : 'a) =
    Assert.AreEqual(src, (roundtripCB src : 'a))

let inline testRoundtripFB (src : 'a) =
    Assert.AreEqual(src, (roundtripFB src : 'a))

let inline testRoundtripSP (src : 'a) =
    Assert.AreEqual(src, (roundtripSP src : 'a))

[<TestClass>]
type UnitTest() = 
    let initSingleField = Ty.SingleField
                            ("test")
    let initBasicTypes1 = Ty.BasicTypes1
                            (true, "test", "test", -1y, 10s, 1000, -32L)
    let initBasicTypes2 = Ty.BasicTypes2
                            (1uy, 3200us, 0xFFFFFu, 0xFFFFFFFF1UL, 3.14, 6.28f, 5)
    let initLists       = Ty.Lists
                            (["foo"; "bar"], [[10; 20; 30]; [1; 2; 3]], [initBasicTypes1])
    let initNullable    = Ty.Nullable
                            ([[3.14]], [], ["foo"], [false], [])
    let initBasicTypes  = Ty.BasicTypes
                            (true, "test", "test", 1uy, 3200us, 0xFFFFFu, 0xFFFFFFFF1UL, -1y, 10s, 1000, -32L, 3.14, 6.28f, 5)
    let initContainers  = Ty.Containers
                            (["one", 1ul; "two", 2ul] |> Map.ofList, 
                            set ["one"; "two"])
    let initTests       = Ty.Tests
                            (t1 = initSingleField, 
                             t2 = initBasicTypes1, 
                             t3 = initBasicTypes2, 
                             t4 = initLists, 
                             t5 = initNullable, 
                             t6 = initBasicTypes,
                             t7 = initContainers)
    
    // TODO: no-fields schemas not supported
    

    // SingleField
    [<TestMethod>]
    member x.CB_TestSingleField () = 
        testRoundtripCB initSingleField

    [<TestMethod>]
    member x.SP_TestSingleField () = 
        testRoundtripSP initSingleField

    [<TestMethod>]
    member x.FB_TestSingleField () = 
        testRoundtripFB initSingleField

    // BasicTypes
    [<TestMethod>]
    member x.CB_TestBasicTypes () = 
        testRoundtripCB initBasicTypes

    [<TestMethod>]
    member x.SP_TestBasicTypes () = 
        testRoundtripSP initBasicTypes

    [<TestMethod>]
    member x.FB_TestBasicTypes () = 
        testRoundtripFB initBasicTypes
    
    // Lists
    [<TestMethod>]
    member x.CB_TestLists () = 
        testRoundtripCB initLists

    [<TestMethod>]
    member x.SP_TestLists () = 
        testRoundtripSP initLists

    [<TestMethod>]
    member x.FB_TestLists () = 
        testRoundtripFB initLists

    // Nullable
    [<TestMethod>]
    member x.CB_TestNullable () = 
        testRoundtripCB initNullable

    [<TestMethod>]
    member x.SP_TestNullable () = 
        testRoundtripSP initNullable

    [<TestMethod>]
    member x.FB_TestNullable () = 
        testRoundtripFB initNullable

    // Tests
    [<TestMethod>]
    member x.CB_TestTests () = 
        testRoundtripCB initTests

    [<TestMethod>]
    member x.SP_TestTests () = 
        testRoundtripSP initTests

    [<TestMethod>]
    member x.FB_TestTests () = 
        testRoundtripFB initTests
    
    // Containers
    [<TestMethod>]
    member x.CB_TestContainers () = 
        testRoundtripCB initContainers

    [<TestMethod>]
    member x.SP_TestContainers () = 
        testRoundtripSP initContainers

    [<TestMethod>]
    member x.FB_TestContainers () = 
        testRoundtripFB initContainers

    // Empty Containers
    [<TestMethod>]
    member x.CB_TestEmptyContainers () = 
        testRoundtripCB (Ty.Containers())

    [<TestMethod>]
    member x.SP_TestEmptyContainers () = 
        testRoundtripSP (Ty.Containers())

    [<TestMethod>]
    member x.FB_TestEmptyContainers () = 
        testRoundtripFB (Ty.Containers())
        
    [<TestMethod>]
    member x.TestDefaultValues () = 
        // Verify Fs default values
        let initDefault = Ty.BasicTypes()
        Assert.AreEqual(true, initDefault.m_bool)
        Assert.AreEqual("foo", initDefault.m_str)
        Assert.AreEqual("bar", initDefault.m_wstr)
        Assert.AreEqual(-14y, initDefault.m_int8)
        Assert.AreEqual(15s, initDefault.m_int16)
        Assert.AreEqual(-16, initDefault.m_int32)
        Assert.AreEqual(-17L, initDefault.m_int64)
        Assert.AreEqual(10uy, initDefault.m_uint8)
        Assert.AreEqual(11us, initDefault.m_uint16)
        Assert.AreEqual(12u, initDefault.m_uint32)
        Assert.AreEqual(13UL, initDefault.m_uint64)
        Assert.AreEqual(18.0, initDefault.m_double)
        Assert.AreEqual(20.0f, initDefault.m_float)
        Assert.AreEqual(int32 -10, initDefault.m_enum1)

    [<TestMethod>]
    member x.CB_TestViews () = 
        let src = initBasicTypes
        let dst = roundtripCB src : Ty.BasicTypes2
        Assert.AreEqual(src.m_uint8, dst.m_uint8)
        Assert.AreEqual(src.m_uint16, dst.m_uint16)
        Assert.AreEqual(src.m_uint32, dst.m_uint32)
        Assert.AreEqual(src.m_uint64, dst.m_uint64)
        Assert.AreEqual(src.m_double, dst.m_double)
        Assert.AreEqual(src.m_float, dst.m_float)
        Assert.AreEqual(src.m_enum1, dst.m_enum1)
        let dst = roundtripCB src : Ty.BasicTypes1
        Assert.AreEqual(src.m_bool, dst.m_bool)
        Assert.AreEqual(src.m_str, dst.m_str)
        Assert.AreEqual(src.m_wstr, dst.m_wstr)
        Assert.AreEqual(src.m_int8, dst.m_int8)
        Assert.AreEqual(src.m_int16, dst.m_int16)
        Assert.AreEqual(src.m_int32, dst.m_int32)
        Assert.AreEqual(src.m_int64, dst.m_int64)

    [<TestMethod>]
    member x.FB_TestViews () = 
        let src = initBasicTypes
        let dst = roundtripFB src : Ty.BasicTypes2
        Assert.AreEqual(src.m_uint8, dst.m_uint8)
        Assert.AreEqual(src.m_uint16, dst.m_uint16)
        Assert.AreEqual(src.m_uint32, dst.m_uint32)
        Assert.AreEqual(src.m_uint64, dst.m_uint64)
        Assert.AreEqual(src.m_double, dst.m_double)
        Assert.AreEqual(src.m_float, dst.m_float)
        Assert.AreEqual(src.m_enum1, dst.m_enum1)
        let dst = roundtripFB src : Ty.BasicTypes1
        Assert.AreEqual(src.m_bool, dst.m_bool)
        Assert.AreEqual(src.m_str, dst.m_str)
        Assert.AreEqual(src.m_wstr, dst.m_wstr)
        Assert.AreEqual(src.m_int8, dst.m_int8)
        Assert.AreEqual(src.m_int16, dst.m_int16)
        Assert.AreEqual(src.m_int32, dst.m_int32)
        Assert.AreEqual(src.m_int64, dst.m_int64)

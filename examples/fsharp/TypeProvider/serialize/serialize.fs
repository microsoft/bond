open System
open Bond.Protocols
open Bond.IO.Unsafe
open Bond.TypeProvider

// Use type provider to create types from a file with marshaled runtime schema
type Ty = SchemaTypeProvider< @"example.schema.serialize.Example">

let serialize (obj : Ty.Example) =
    let output = new OutputBuffer()
    let writer = new CompactBinaryWriter<OutputBuffer>(output) 
    obj.SerializeTo(writer)
    output.Data

let deserialize (data : ArraySegment<byte>) =
    let reader = new CompactBinaryReader<InputBuffer>(new InputBuffer(data))
    Ty.Example.DeserializeFrom(reader)

[<EntryPoint>]
let main argv = 
    let src = Ty.Example("math", [Ty.Const("Pi", 3.14); Ty.Const("Tau", 6.28)])
    let dst = serialize src |> deserialize 
    printfn "%A" dst
    assert (dst.name = "math")
    assert (dst.consts.Head.name = "Pi")
    assert (dst.consts.Head.value = 3.14)
    0 // return an integer exit code
    

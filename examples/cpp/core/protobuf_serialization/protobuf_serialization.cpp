#include "protobuf_serialization_reflection.h"
#include "protobuf_serialization.pb.h"

#include <bond/core/bond.h>
#include <bond/stream/output_buffer.h>
#include <bond/protocol/protobuf_binary_writer.h>

#include <google/protobuf/util/message_differencer.h>

#include <cassert>


int main()
{
    // Fill bond struct
    examples::protobuf::serialization::Item bond_item1;
    bond_item1.ui32 = 1000;
    bond_item1.fu64 = 12345678ULL;
    bond_item1.str = "Item1";
    bond_item1.vsi32.push_back(100);
    bond_item1.vsi32.push_back(-200);
    bond_item1.vd.push_back(1.2);
    examples::protobuf::serialization::Item bond_item2;
    bond_item2.ui32 = 2000;
    bond_item2.fu64 = 87654321ULL;
    bond_item2.str = "Item2";
    bond_item2.vsi32.push_back(-100);
    bond_item2.vsi32.push_back(200);
    bond_item2.vd.push_back(3.4);
    bond_item2.vd.push_back(5.6);
    examples::protobuf::serialization::Struct bond_struct;
    bond_struct.item.set(bond_item1);
    bond_struct.items[10] = bond_item1;
    bond_struct.items[20] = bond_item2;

    // Fill proto message
    examples::protobuf::proto::serialization::Item proto_item1;
    proto_item1.set_ui32(bond_item1.ui32);
    proto_item1.set_fu64(bond_item1.fu64);
    proto_item1.set_str(bond_item1.str);
    proto_item1.add_vsi32(bond_item1.vsi32[0]);
    proto_item1.add_vsi32(bond_item1.vsi32[1]);
    proto_item1.add_vd(bond_item1.vd[0]);
    examples::protobuf::proto::serialization::Item proto_item2;
    proto_item2.set_ui32(bond_item2.ui32);
    proto_item2.set_fu64(bond_item2.fu64);
    proto_item2.set_str(bond_item2.str);
    proto_item2.add_vsi32(bond_item2.vsi32[0]);
    proto_item2.add_vsi32(bond_item2.vsi32[1]);
    proto_item2.add_vd(bond_item2.vd[0]);
    proto_item2.add_vd(bond_item2.vd[1]);
    examples::protobuf::proto::serialization::Struct proto_struct;
    *proto_struct.mutable_item() = proto_item1;
    (*proto_struct.mutable_items())[10] = proto_item1;
    (*proto_struct.mutable_items())[20] = proto_item2;

    // Serialize bond struct
    bond::OutputBuffer output;
    bond::ProtobufBinaryWriter<bond::OutputBuffer> writer(output);
    bond::Serialize(bond_struct, writer);

    bond::blob data = output.GetBuffer();

    // Deserialize back the proto struct
    examples::protobuf::proto::serialization::Struct proto_struct2;
    if (!proto_struct2.ParseFromArray(data.data(), static_cast<int>(data.size())))
    {
        assert(false);
    }

    // Content must be equal
    if (!google::protobuf::util::MessageDifferencer::Equals(proto_struct, proto_struct2))
    {
        assert(false);
    }

    return 0;
}

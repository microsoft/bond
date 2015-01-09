#include "generic_tree_reflection.h"

#include <bond/core/bond.h>
#include <bond/stream/output_buffer.h>

using namespace examples::generic_tree;


int main()
{
    // Define root node for a tree of strings
    Node<std::string> root;

    root.data = "root";
    root.left.set().data = "root/left";
    root.right.set().data = "root/right";
    root.left->left.set().data = "root/left/left";
    root.left->left->right.set().data = "root/left/left/right";

    bond::OutputBuffer output;
    bond::CompactBinaryWriter<bond::OutputBuffer> writer(output);

    // Serialize the tree
    Marshal(root, writer);

    bond::InputBuffer input(output.GetBuffer());
                         
    // De-serialize the tree
    Node<std::string> tree;

    Unmarshal(input, tree);
    
    return 0;    
}

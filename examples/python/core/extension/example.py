from __future__ import print_function
import hashlib
import python_extension as extension

def person(first, last, eyes):
    # Create and initialize an instance of Person struct
    obj = extension.Person()
    obj.first_name = first
    obj.last_name = last
    obj.eyes = eyes
    m = hashlib.md5()
    m.update('{}::{}::{}'.format(first, last, eyes).encode('utf8'))
    obj.hash = m.digest()
    return obj

def main():
    # Create an instance of Example struct
    obj = extension.Example()

    # Initialize map using dictionary comprehension
    obj.people = {i: person(first, last, eyes) for i, first, last, eyes in \
            [(1, "Carlos", "Danger", extension.Color.Red)]}

    # Print JSON representation
    print(extension.Serialize(obj, extension.ProtocolType.SIMPLE_JSON_PROTOCOL))

    # Serialize and deserialize using default protocol (Compact Binary)
    data = extension.Serialize(obj)
    new = extension.Example()
    extension.Deserialize(data, new)
    assert(new == obj)

    # Print JSON representation
    print(extension.Serialize(new, extension.ProtocolType.SIMPLE_JSON_PROTOCOL))

if __name__ == '__main__':
    main()


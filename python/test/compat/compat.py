from __future__ import print_function
import python_compatibility_test as test
import sys, getopt

def schema(input_, output):
    obj = test.SchemaDef()
    schema = test.GetRuntimeSchema(test.Compat())

    test.Unmarshal(input_.read(), obj)
    assert(obj == schema)
    output.write(test.Marshal(schema))

def compat(input_, output, test_case):
    obj = test.Compat()

    protocol = test.ProtocolType.COMPACT_PROTOCOL
    if (test_case == "json"):
        protocol = test.ProtocolType.SIMPLE_JSON_PROTOCOL

    test.Deserialize(input_.read(), obj, protocol)
    output.write(test.Serialize(obj, protocol))

def usage():
    print('')
    print('compat.py -d INPUT_FILE -s OUTPUT_FILE TEST')
    print('')
    print(' -? --help                      show this help text')
    print('    TEST                        compact | json | schema')
    print(' -d --deserialize=INPUT_FILE    deserialize object from specified file')
    print(' -s --serialize=OUTPUT_FILE     serialize object to specified file')

def main(argv):
    inputfile = ''
    outputfile = ''
    try:
        opts, args = getopt.getopt(argv,"?s:d:",["help","serialize=","deserialize="])
    except getopt.GetoptError:
        usage()
        sys.exit(2)

    for opt, arg in opts:
        if opt in ("-?", "--help"):
            usage()
            sys.exit()
        elif opt in ("-s", "--serialize"):
            outputfile = arg
        elif opt in ("-d", "--deserialize"):
            inputfile = arg

    if (len(args) != 1 \
     or args[0] not in ("compact", "json", "schema")\
     or inputfile == ""
     or outputfile == ""):
        usage()
        sys.exit(2)

    input_ = open(inputfile, "rb")
    output = open(outputfile, "wb+")

    if (args[0] == "schema"):
        schema(input_, output)
    else:
        compat(input_, output, args[0])

if __name__ == "__main__":
   main(sys.argv[1:])

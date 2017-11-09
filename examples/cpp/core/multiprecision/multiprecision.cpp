#include "multiprecision_reflection.h"

#include <bond/core/bond.h>
#include <bond/stream/output_buffer.h>

#include <boost/multiprecision/miller_rabin.hpp>

using namespace examples::multiprecision;

int main()
{
    using namespace boost::random;

    mt11213b gen1(clock());
    mt19937 gen2(clock());
    independent_bits_engine<mt11213b, 512, uint512> gen512(gen1);
    independent_bits_engine<mt11213b, 1024, uint1024> gen1024(gen1);

    Example obj, obj2;

    // Generate some large random primes
    // Note that the literature recommends 25 trials of Miller-Rabin for a good
    // likelihood that a number is prime but this is just a silly little
    // example and we want it to run relatively fast so doing just one.
    while (!miller_rabin_test(obj.public_key, 1, gen2))
        obj.public_key = gen512();

    while (!miller_rabin_test(obj.private_key, 1, gen2))
        obj.private_key = gen1024();

    // Arbitrary precision math
    obj.product = obj.public_key * obj.private_key;

    // Serialize the object
    bond::OutputBuffer output;
    bond::CompactBinaryWriter<bond::OutputBuffer> writer(output);
    Serialize(obj, writer);

    // De-serialize the object
    bond::CompactBinaryReader<bond::InputBuffer> reader(output.GetBuffer());
    Deserialize(reader, obj2);

    BOOST_ASSERT(obj2 == obj);

    return 0;
}

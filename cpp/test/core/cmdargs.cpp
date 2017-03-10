#include "precompiled.h"
#include "cmdargs.h"

#include <bond/core/cmdargs.h>
#include <boost/mpl/for_each.hpp>
#include <boost/foreach.hpp>

#include <cmdargs_types.h>
#include <cmdargs_reflection.h>

using namespace unit_test;
using namespace std;

typedef boost::mpl::push_front
    <
        NumericTypes,
        string
    >::type
    PrimitiveTypes;

typedef boost::mpl::list
    <
        EnumType
    >
    EnumTypes;


const char* ParameterVariants[] = {"--field=", "--field:", "-f ", "-xf "};
const char* FlagVariants[] = {"--field", "-f", "-xf", "-fx"};
const char* SpecialStrings[] = {":", "=", "-", "--", "\"- -\"", "\" - \"", "\"-\"", "\" \"", "\"\""};
const char* Failures[] = {"--flag=ValueForFlag", "--unknown:field", "--en=BadEnum", "--i32:3.14", "--d=NotDouble"};


namespace boost
{
    // Overload lexical_cast for enum
    template <typename T>
    typename enable_if<std::is_enum<T>, T>::type
    lexical_cast(int n)
    {
        return static_cast<T>(n);
    }

    // Overload lexical_cast for bool
    template <typename T>
    typename enable_if<std::is_same<T, bool>, T>::type
    lexical_cast(int n)
    {
            return !!n;
    }
}


// Enum types already have generated overload for ToString.
// For other types we use boost::lexical_cast.
template <typename T>
std::string ToString(const T& value)
{
    return boost::lexical_cast<std::string>(value);
}


// CmdArg
struct CmdArg : boost::noncopyable
{
    CmdArg(int variant = 0xffff)
        : variant(variant),
          before(false),
          after(false)
    {
        _Add("example.exe");

        AddBefore();
    }

    // Get options
    template <typename T>
    void Get(T& options)
    {
        AddAfter();

        options = bond::cmd::GetArgs<T>(argc(), argv());
    }

    // Single param
    template <typename T>
    void Add(const std::string& param, const T& value)
    {
        Add(param + ToString(value));
    }

    template <typename T>
    void AddRaw(const std::string& param, const T& value)
    {
        _Add(param + ToString(value));
    }

    // Comma-separated list
    template <typename T>
    void Add(std::string param, const std::list<T>& value)
    {
        for (const auto& item : value)
        {
            param += ToString(item) + ",";
        }

        // remove trailing comma
        param = param.substr(0, param.size() - 1);

        Add(param);
    }

    // Add param, splitting on the first space before quote (if any)
    void Add(const std::string& param)
    {
        size_t space = param.find(' ');
        size_t quote = param.find('"');

        if (space > quote)
            space = std::string::npos;

        _Add(param.substr(0, space));

        if (space != std::string::npos)
            _Add(param.substr(space + 1));
    }

private:
    int argc() const
    {
        return static_cast<int>(params.size());
    }

    char** argv() const
    {
        return const_cast<char**>(&pointers[0]);
    }

    void AddBefore()
    {
        BOOST_ASSERT (!before);
        before = true;

        switch (variant)
        {
            case 0:
                _Add("-b");
                _Add("234");
                _Add("--after2:Value3");
                break;

            case 1:
                _Add("--before1:1032");
                _Add("--before2=\"one two three\"");
                break;

            case 2:
                break;

            case 3:
                _Add("--before1:0");
                _Add("--before2=\"one two three\"");
                _Add("--before3");
                break;
        }
    }

    void AddAfter()
    {
        BOOST_ASSERT (!after);
        after = true;

        switch (variant)
        {
            case 0:
                _Add("--before2=one");
                _Add("-a");
                _Add("0");
                break;

            case 1:
                _Add("--after1:10.32");
                _Add("--after2=Value2");
                _Add("--before3");
                break;

            case 2:
                _Add("--after1:10.32");
                _Add("--after2=Value3");
                break;

            case 3:
                break;
        }
    }

    void _Add(const std::string& param)
    {
        params.push_back(param);
        pointers.push_back(params.back().c_str());
    }

    std::vector<const char*> pointers;
    std::list<std::string> params;
    int variant;
    bool before;
    bool after;
};


// SingleParam
struct SingleParam
{
    template <typename T>
    void operator()(const T&)
    {
        T input = boost::lexical_cast<T>(1);

        for (int i = 0; i <= 4; ++i)
        {
            Test(input, i);
            TestNaked(input, i);
        }
    }

    template <typename T>
    void Test(const T& input, int variant)
    {
        // Flags, e.g.:
        //  --field=1
        //  --field:Value3
        //  -f 0
        for (const auto& param : ParameterVariants)
        {
            CmdArg data(variant);
            Option<T> options;

            data.Add(param, input);
            data.Get(options);

            UT_AssertIsTrue(input == options.field);
        }
    }

    void Test(const bool&, int variant)
    {
        // Flags, e.g.:
        //  --field
        //  -f
        for (const auto& flag : FlagVariants)
        {
            CmdArg data(variant);
            Option<bool> options;

            data.Add(flag);
            data.Get(options);

            UT_AssertIsTrue(options.field);
        }
    }


    void TestNaked(const bool&, int)
    {
        // naked bool not supported
    }

    template <typename T>
    void TestNaked(const T& input, int variant)
    {
        // Naked, e.g.:
        //  0
        //  Value2
        CmdArg data(variant);
        Naked<T> naked;

        data.Add("", input);
        data.Get(naked);

        UT_AssertIsTrue(input == naked.field);
    }
};


// ParamList
struct ParamList
{
    template <typename T>
    void operator()(const T&)
    {
        T input = boost::lexical_cast<T>(1);

        for (int i = 0; i <= 4; ++i)
            Test(input, i);
    }

    void operator()(const bool&)
    {
        // list<bool> not supported
    }

    template <typename T>
    void Test(const T&, int variant)
    {
        list<T> input;

        input.push_back(boost::lexical_cast<T>(0));
        input.push_back(boost::lexical_cast<T>(1));
        input.push_back(boost::lexical_cast<T>(2));

        // Flags - list item per param, e.g.:
        //  -f 0 -f 1 -f 2
        for (std::string param : ParameterVariants)
        {
            if (param == "-xf ")
                continue;

            CmdArg data(variant);
            Option<list<T> > options;

            for (const auto& item : input)
            {
                data.Add(param, item);
            }

            data.Get(options);

            UT_AssertIsTrue(input == options.field);
        }

        // variant 0 not supported for naked list because they are greedy
        if (variant != 0)
        {
            // Naked - list item per param, e.g.:
            //  0 1 2
            CmdArg data(variant);
            Naked<list<T> > naked;

            for (const auto& item : input)
            {
                data.Add("", item);
            }

            data.Get(naked);

            UT_AssertIsTrue(input == naked.field);
        }

        // List per param, e.g.:
        //  --field=0,1,2
        //  Value1 Value2 Value3
        SingleParam single;

        single.Test(input, variant);

        // variant 0 not supported for naked list because they are greedy
        if (variant != 0)
            single.TestNaked(input, variant);
    }
};


TEST_CASE_BEGIN(TypeTests)
{
    boost::mpl::for_each<PrimitiveTypes>(SingleParam());
}
TEST_CASE_END


TEST_CASE_BEGIN(EnumTests)
{
    boost::mpl::for_each<EnumTypes>(SingleParam());
}
TEST_CASE_END


TEST_CASE_BEGIN(ListTests)
{
    boost::mpl::for_each<PrimitiveTypes>(ParamList());
    boost::mpl::for_each<EnumTypes>(ParamList());

    // list tokenizer
    for (const auto& param : ParameterVariants)
    {
        {
            Option<std::vector<std::string> > options;
            CmdArg data(0);

            data.Add(param, "\\,,\\\"");
            data.Get(options);

            UT_AssertIsTrue(2 == options.field.size());
            UT_AssertIsTrue("," == options.field[0]);
            UT_AssertIsTrue("\"" == options.field[1]);
        }

        {
            Option<std::vector<std::string> > options;
            CmdArg data(0);

            data.AddRaw("--field=", "one two,three");
            data.Get(options);

            UT_AssertIsTrue(2 == options.field.size());
            UT_AssertIsTrue("one two" == options.field[0]);
            UT_AssertIsTrue("three" == options.field[1]);
        }

        {
            Option<std::vector<std::string> > options;
            CmdArg data(0);

            data.AddRaw("--field=", "\"one two,three\"");
            data.Get(options);

            UT_AssertIsTrue(1 == options.field.size());
            UT_AssertIsTrue("one two,three" == options.field[0]);
        }
    }
}
TEST_CASE_END


TEST_CASE_BEGIN(SpecialTests)
{
    for (const auto& param : ParameterVariants)
    {
        for (std::string value: SpecialStrings)
        {
            for (int variant = 0; variant <= 4; ++variant)
            {
                {
                    CmdArg data(variant);
                    Option<std::string> options;

                    data.Add(param, value);
                    data.Get(options);

                    UT_AssertIsTrue(value == options.field);
                }

                {
                    CmdArg data(variant);
                    Naked<std::string> naked;

                    data.Add("", value);
                    data.Get(naked);

                    UT_AssertIsTrue(value == naked.field);
                }

                {
                    CmdArg data(variant);
                    Naked<std::vector<std::string> > naked;

                    data.Add("", value);
                    data.Get(naked);

                    std::string element;
                    std::remove_copy(value.begin(), value.end(), std::back_inserter(element), '"');

                    UT_AssertIsTrue(element == naked.field[0]);
                }
            }
        }
    }
}
TEST_CASE_END


TEST_CASE_BEGIN(FailureTests)
{
    for (const auto& param : Failures)
    {
        // invalid types, unexpected arguments, invalind enum
        CmdArg data;
        Failure options;

        data.Add("naked");
        data.Add("--need=required");
        data.Add(param);

        UT_AssertThrows(data.Get(options), std::exception);
    }

    for (const auto& value : SpecialStrings)
    {
        // special string as unexpected command line argument
        CmdArg data;
        Failure options;

        data.Add("naked");
        data.Add("--need=required");
        data.Add("", value);

        UT_AssertThrows(data.Get(options), std::exception);
    }

    {
        // missing required flag
        CmdArg data;
        Failure options;

        data.Add("naked");

        UT_AssertThrows(data.Get(options), std::exception);
    }

    {
        // missing required naked
        CmdArg data;
        Failure options;

        data.Add("--need=required");

        UT_AssertThrows(data.Get(options), std::exception);
    }
}
TEST_CASE_END


TEST_CASE_BEGIN(UsageTests)
{
    std::string expected =
"\n"
"Usage: example.exe [options] [NAKED]\n"
"\n"
"    --what=WHAT               this is an example of Usage output from CmdArg   \n"
"                              library\n"
"    --flag                    boolean flag\n"
" -e --enumerator=ENUMERATOR   Value1 | Value2 | Value3, default Value1\n"
"    --items=ITEMS             comma-separated list of: Value1 | Value2 | Value3\n"
"    --num=NUM                 default 0\n"
" -l --longish=LONGISH         long help string that will need to be wrapped    \n"
"                              because it won't fit in one line\n"
"    NAKED                     naked parameter doesn't need --flag in front of  \n"
"                              it, default 0\n";

    std::ostringstream actual;
    Usage options;
    Apply(bond::cmd::detail::Usage("example.exe", actual), options);

    UT_AssertAreEqual(expected, actual.str());
}
TEST_CASE_END


void CmdArgs::Initialize()
{
    UnitTestSuite suite("Command line arguments");

    AddTestCase<TEST_ID(0x2401), TypeTests>(suite, "basic type arguments");
    AddTestCase<TEST_ID(0x2401), EnumTests>(suite, "enum arguments");
    AddTestCase<TEST_ID(0x2401), ListTests>(suite, "list arguments");
    AddTestCase<TEST_ID(0x2401), SpecialTests>(suite, "special characters on command line");
    AddTestCase<TEST_ID(0x2401), FailureTests>(suite, "argument failures");
    AddTestCase<TEST_ID(0x2401), UsageTests>(suite, "usage help");
}


bool init_unit_test()
{
    CmdArgs::Initialize();
    return true;
}

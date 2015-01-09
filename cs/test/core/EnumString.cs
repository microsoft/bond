namespace Alias
{
    public class EnumString<T>
    {
        public string Value { get; set; }

        public EnumString()
        {
            Value = string.Empty;
        }

        public EnumString(T value)
        {
            Value = value.ToString();
        }

        public EnumString(string value)
        {
            Value = value;
        }

        public override int GetHashCode()
        {
            return (Value != null ? Value.GetHashCode() : 0);
        }

        public override bool Equals(object that)
        {
            return string.Equals(Value, (that as EnumString<T>).Value);
        }

        public static bool operator ==(EnumString<T> left, EnumString<T> right)
        {
            return left.Equals(right);
        }

        public static bool operator !=(EnumString<T> left, EnumString<T> right)
        {
            return !(left == right);
        }
    }

    public static class BondTypeAliasConverter
    {
        public static string Convert<T>(EnumString<T> value, string unused)
        {
            return value.Value;
        }

        public static EnumString<T> Convert<T>(string value, EnumString<T> unused)
        {
            return new EnumString<T>(value);
        }
    }
}

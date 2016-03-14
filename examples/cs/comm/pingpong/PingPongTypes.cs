namespace Bond.Examples.PingPong
{
    using System;

    [Schema]
    public class PingRequest
    {
        [Bond.Id(0)]
        public string Payload { get; set; } = string.Empty;

        [Bond.Id(1)]
        public UInt16 DelayMilliseconds { get; set; }
    }

    [Schema]
    public class PingResponse
    {
        [Bond.Id(0)]
        public string ResponsePayload { get; set; } = string.Empty;
    }
}

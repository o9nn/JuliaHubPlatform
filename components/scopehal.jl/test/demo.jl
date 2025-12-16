using scopehal
using Plots

t = scopehal.CreateTransport("null", "")
@assert scopehal.IsConnected(t)
o = scopehal.CreateOscilloscope("demo", t)

ch = scopehal.GetChannel(o, 0)
# scopehal.ForceTrigger(o)
println("acquiring data")
scopehal.Start(o)
while scopehal.PollTrigger(o) != scopehal.TRIGGER_MODE_TRIGGERED
    print(".")
end
scopehal.AcquireData(o)

while scopehal.HasPendingWaveforms(o)
    scopehal.PopPendingWaveform(o)
end

println("ready to get data")
d = scopehal.AnalogWaveformData(scopehal.GetData(ch, 0))
plot(d)
using ModelingToolkitStandardLibrary.Electrical
using ModelingToolkitStandardLibrary.Blocks: Constant
using SymbolicIndexingInterface
@testset "Block Modeling" begin 
    @testset "RC Functional" begin 
        R = 1.0
        C = 1.0
        V = 1.0
        @variables t
        @named resistor = Resistor(R = R)
        @named capacitor = Capacitor(C = C)
        @named source = Voltage()
        @named constant = Constant(k = V)
        @named ground = Ground()

        @named sensor = PotentialSensor()
        @named key_parameter = MeasureComponent()

        rc_eqs = [connect(constant.output, source.V)
                connect(source.p, resistor.p)
                connect(resistor.n, capacitor.p)
                connect(capacitor.n, source.n, ground.g)
                connect(sensor.p, capacitor.p)
                sensor.phi ~ key_parameter.value]

        @named rc_model = ODESystem(rc_eqs, t,
            systems = [resistor, capacitor, constant, source, ground, sensor, key_parameter])
        sys = structural_simplify(rc_model)
        prob1 = ODEProblem(sys, Pair[], (0, 10.0))
        sol1 = solve(prob1, Tsit5())
        prob2 = ODEProblem(sys, Pair[capacitor.C => 0.9], (0, 10.0)) # this converges to nearly the same solution but is a little too fast
        sol2 = solve(prob2, Tsit5())
        prob3 = ODEProblem(sys, Pair[capacitor.C => 5.0], (0, 10.0)) # this doesn't stabilize in the allotted time
        sol3 = solve(prob3, Tsit5())

        d1 = discretize_solution(sol1, sol1)
        ds1 = discretize_solution(sol1, sol1; measured=SymbolicIndexingInterface.all_variable_symbols(sys))

        @test sum(compare(sol1, ds1; warn_observed=false)[:, :L∞]) < 0.01
        @test sum(compare(sol2, ds1; warn_observed=false)[:, :L∞]) > sum(compare(sol1, ds1; warn_observed=false)[:, :L∞])
        @test sum(compare(sol3, ds1; warn_observed=false)[:, :L∞]) > sum(compare(sol2, ds1; warn_observed=false)[:, :L∞])
        @test sum(compare(sol1, ds1; warn_observed=false)[:, :L∞]) > sum(compare(sol1, d1; warn_observed=false)[:, :L∞])
        @test sum(compare(sol2, ds1; warn_observed=false)[:, :L∞]) > sum(compare(sol2, d1; warn_observed=false)[:, :L∞])
        @test sum(compare(sol3, ds1; warn_observed=false)[:, :L∞]) > sum(compare(sol3, d1; warn_observed=false)[:, :L∞])

        # construct a fictional power measurement
        power_synth = select(ds1, :timestamp, ["capacitor₊v(t)", "capacitor₊i(t)"] => ((v, i) -> v .* i) => "power")
        @test compare(sol1, power_synth, [capacitor.i * capacitor.v => "power" => "power"])[1, "L∞"] < 0.01
        @test compare(sol2, power_synth, [capacitor.i * capacitor.v => "power" => "power"])[1, "L∞"] < 0.05
        @test compare(sol3, power_synth, [capacitor.i * capacitor.v => "power" => "power"])[1, "L∞"] < 0.3
        
    end
end

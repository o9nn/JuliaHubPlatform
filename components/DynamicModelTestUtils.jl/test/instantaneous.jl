@testset "Instantaneous" begin 
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

        rc_eqs = [connect(constant.output, source.V)
                connect(source.p, resistor.p)
                connect(resistor.n, capacitor.p)
                connect(capacitor.n, source.n, ground.g)
                connect(sensor.p, capacitor.p)]

        @named rc_model = ODESystem(rc_eqs, t,
            systems = [resistor, capacitor, constant, source, ground, sensor])
        sys = structural_simplify(rc_model)
        @test test_instantaneous(sys, [], [resistor.v]; t = 0.0)[1] > 0
        @test test_instantaneous(sys, [], [resistor.i]; t = 0.0)[1] > 0
        @test test_instantaneous(sys, [constant.k => 0.0], [resistor.v]; t = 0.0)[1] == 0
        @test test_instantaneous(sys, [constant.k => 5.0], [resistor.v]; t = 0.0)[1] == 5
    end
end
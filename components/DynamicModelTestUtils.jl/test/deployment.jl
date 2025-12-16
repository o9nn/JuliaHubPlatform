using Git, CSV
@testset "deploy" begin
    @testset "bare" begin 
        mktempdir() do dir
            cd(dir) do 
                mkdir("repo")
                run(`$(git()) -C repo init -q --bare`)
                full_repo_path = joinpath(pwd(), "repo")
                mkdir("runs")
                write("runs/run.csv", "1,2,3")
                DynamicModelTestUtils.deploy(
                    pwd(), "runs",
                    deploy_type = DynamicModelTestUtils.FilesystemDeployConfig(full_repo_path, "."),
                    repo = full_repo_path
                )
                run(`$(git()) clone -q -b data $(full_repo_path) worktree`)
                println(readdir("worktree/"))
                @test isfile(joinpath("worktree", "run.csv"))
                DynamicModelTestUtils.load_data(root=full_repo_path) do localdir
                    @test isfile("run.csv")
                end
            end
        end
    end
    @testset "RC deploy" begin 
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
        sol = solve(prob1, Tsit5())
    
        d1 = discretize_solution(sol)
    
        mktempdir() do dir
            cd(dir) do 
                mkdir("repo")
                run(`$(git()) -C repo init -q --bare`)
                full_repo_path = joinpath(pwd(), "repo")
                mkdir("data")
                CSV.write("data/output.csv", d1)
                DynamicModelTestUtils.deploy(
                    pwd(), "data",
                    deploy_type = DynamicModelTestUtils.FilesystemDeployConfig(full_repo_path, "."),
                    repo = full_repo_path,
                )
                DynamicModelTestUtils.load_data(root=full_repo_path) do localdir
                    @test isfile("output.csv")
                    restored = CSV.read("output.csv", DataFrame)
                    @test sum(compare(sol, restored)[:, :L∞]) < 0.01
                end
            end
        end
    end
    @testset "RC regression" begin 
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
        sol = solve(prob1, Tsit5())
        prob2 = ODEProblem(sys, Pair[capacitor.C => 0.5], (0, 10.0))
        sol2 = solve(prob2, Tsit5())
        
        println("\n\n\nstart deploy test")
        mktempdir() do dir
            cd(dir) do 
                mkdir("repo")
                run(`$(git()) -C repo init -q --bare`)
                full_repo_path = joinpath(pwd(), "repo")
                DynamicModelTestUtils.regression_test(Dict(:rc => sol), full_repo_path, "data", true; 
                    deploy_only = true,
                    make_output_dir = true)
                println("deployed! to $full_repo_path")
                run(`$(git()) clone -q -b data $(full_repo_path) worktree`)
                println(readdir("worktree/"))
                DynamicModelTestUtils.regression_test(Dict(
                    :rc => sol
                ), full_repo_path)
                #DynamicModelTestUtils.regression_test(Dict(
                #    :rc => sol2
                #), full_repo_path)
            end
        end
    end
end
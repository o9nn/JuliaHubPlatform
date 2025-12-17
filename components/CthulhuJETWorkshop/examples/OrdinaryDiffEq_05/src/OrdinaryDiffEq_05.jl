module OrdinaryDiffEq_05

using OrdinaryDiffEqSDIRK
using OrdinaryDiffEqCore
using ADTypes

function lorenz(du, u, p, t)
    du[1] = 10.0(u[2] - u[1])
    du[2] = u[1] * (28.0 - u[3]) - u[2]
    du[3] = u[1] * u[2] - (8 / 3) * u[3]
end
const u0 = [1.0; 0.0; 0.0]
const tspan = (0.0, 100.0)

function @main(ARGS)
    prob = ODEProblem(ODEFunction{true, SciMLBase.FullSpecialize}(lorenz), u0, tspan)
    solver = ImplicitEuler(autodiff=AutoForwardDiff(; chunksize=1))
    sol = solve(prob, solver)
    println(Core.stdout, sol.t[end])
    println(Core.stdout, sum(sol.u[end]))

    return 0
end

end # module OrdinaryDiffEq_05

@static if Base.JLOptions().trim != 0
    using .OrdinaryDiffEq_05: main
    using OrdinaryDiffEqSDIRK

    @eval OrdinaryDiffEqSDIRK begin
        # OrdinaryDiffEqCore.ispredictive(alg::OrdinaryDiffEqNewtonAdaptiveAlgorithm) = false
        # OrdinaryDiffEqCore.isstandard(alg::OrdinaryDiffEqNewtonAdaptiveAlgorithm) = false
    end
end

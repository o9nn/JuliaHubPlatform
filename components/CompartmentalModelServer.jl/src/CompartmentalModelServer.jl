module CompartmentalModelServer

using ModelingToolkit
using OrdinaryDiffEq
using StructTypes

export greet
export problem
export solution
export simulate

greet() = "Hello World!"

"Model default values of type `NamedTuple{Float64}`."
const DEFAULTS = (; S = 999.0::Float64, I = 1.0::Float64, R = 0.0::Float64,
                  β = (1 / 2)::Float64, γ = (1 / 3)::Float64,
                  tstart = 0.0::Float64, tstop = 30.0::Float64)

@doc raw"""
    system()::ODESystem

Construct the basic epidemiology compartmental model, the SIR model.

```math
\begin{equation}
    \begin{aligned}
        \frac{dS}{dt} &= -{\frac{\beta IS}{N}}, \\
        \frac{dI}{dt} &= {\frac{\beta IS}{N}} - \gamma I, \\
        \frac{dR}{dt} &= \gamma I,
    \end{aligned}
\end{equation}
```
"""
function system()::ODESystem

    # define independent variable and differentiable
    @variables t
    D = Differential(t)

    # define states and parameters
    states = @variables begin
        S(t) = DEFAULTS.S
        I(t) = DEFAULTS.I
        R(t) = DEFAULTS.R
    end
    params = @parameters begin
        β = DEFAULTS.β
        γ = DEFAULTS.γ
    end

    # define equations
    eqns = Equation[D(S) ~ -β * I * S / (S + I + R),
                    D(I) ~ (β * I * S / (S + I + R)) - (γ * I),
                    D(R) ~ γ * I]

    # construct and return ode system
    @named _model = ODESystem(eqns, t, states, params)
    return structural_simplify(_model)
end

"""
    problem(sys = system())::ODEProblem
    
Constructs the original SIR model ode problem using `CompartmentalModelServer.DEFAULTS`.
"""
function problem(sys = system())
    ODEProblem(sys, Float64[], (DEFAULTS.tstart, DEFAULTS.tstop), Float64[])
end

"Holds values for beta and gamma as well as serialization/deserialization information."
struct ModelParameters
    beta::Float64
    gamma::Float64
end

"Holds values complete model solution as well as serialization/deserialization information."
struct ModelSolution
    time::Vector{Float64}
    S::Vector{Float64}
    I::Vector{Float64}
    R::Vector{Float64}
    params::ModelParameters
end

function ModelSolution(sol::ODESolution, prob::ODEProblem)
    ModelSolution(sol.t,
                  sol[:S], sol[:I], sol[:R],
                  ModelParameters(prob.p...))
end

StructTypes.StructType(::Type{ModelSolution}) = StructTypes.Struct()

"""
    solution(prob = problem())::ModelSolution
    
Constructs the original compartmental model ode solution and returns an object that can be
serialized to JSON.
"""
function solution(prob = problem())
    sol = solve(prob, Tsit5())
    ModelSolution(sol, prob)
end

"""
    simulate(; Snew, Inew, Rnew, βnew, γnew, tstart, tstop)::ModelSolution

Returns a new solution for the compartmental model given the initial conditions, parameters
and timespan passed as keyword arguments. The result is a `ModelSolution` object that can be
readily serialized to JSON.
"""
function simulate(; Snew = DEFAULTS.S, Inew = DEFAULTS.I, Rnew = DEFAULTS.R,
                  βnew = DEFAULTS.β, γnew = DEFAULTS.γ,
                  tstart = DEFAULTS.tstart, tstop = DEFAULTS.tstop)

    # use originally defined model to unpack states and parameters
    sys = system()
    t = ModelingToolkit.get_iv(sys)
    @unpack S(t), I(t), R(t) = ModelingToolkit.get_states(sys)
    β, γ = ModelingToolkit.get_ps(sys)

    # construct new problem
    newprob = remake(problem(); u0 = [Snew, Inew, Rnew], tspan = (tstart, tstop),
                     p = [β => βnew, γ => γnew])

    # solve and return
    return solution(newprob)
end

function __init__()
    system()
    problem()
    solution()
    simulate()
end

end # module CompartmentalModelServer

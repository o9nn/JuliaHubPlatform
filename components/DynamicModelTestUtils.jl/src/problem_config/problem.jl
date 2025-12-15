module ProblemHelpers
import SciMLBase
using DataFrames
# some thin wrappers over remake
initial_condition(prob::SciMLBase.AbstractDEProblem, u0) = remake(prob, u0 = u0)
tspan(prob::SciMLBase.AbstractDEProblem, tspan) = remake(prob, tspan = tspan)
parameters(prob::SciMLBase.AbstractDEProblem, ps) = remake(prob, p = ps)

remake_solver_kwarg(prob, merge; kwargs...) = remake(prob; kwargs = mergewith(merge, kwargs, prob.kwargs))
tstops(prob::SciMLBase.AbstractDEProblem, tstops) = remake_solver_kwarg(prob, (new, old) -> sort!([new; old]); tstops = tstops)
tstops(prob::SciMLBase.AbstractDEProblem, tstops::DataFrame) = tstops(prob, collect(tstops[:, :timestamp]))

export initial_condition, tspan, parameters, tstops
end
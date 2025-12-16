
function test_instantaneous(
    sys::ModelingToolkit.AbstractSystem,
    ic,
    checks::Num;
    t = nothing)
    return test_instantaneous(sys, ic, checks; t = t)
end

"""
    test_instantaneous(sys::ModelingToolkit.AbstractSystem,
        ic,
        checks::Union{Num, Array};
        t = nothing)

`test_instantaneous` is a helper wrapper around constructing and executing an `ODEProblem` at a specific condition. It's intended for use in basic
sanity checking of MTK models, for example to ensure that the derivative at a given condition has the correct sign. It should be passed the system,
the initial condition dictionary to be given to the ODEProblem constructor (including parameters), and a list of observable values from the system to
extract from the ODEProblem. If `checks` is a single `Num` then the result will be that observed value; otherwise it will return an array in the same
order as the provided checks.
"""
function test_instantaneous(
    sys::ModelingToolkit.AbstractSystem,
    ic,
    checks::Array;
    t = nothing)
    t = isnothing(t) ? 0.0 : t
    prob = ODEProblem(sys, ic, (0.0, 0.0))
    getter = getu(prob, checks)
    return getter(prob)
end

export test_instantaneous
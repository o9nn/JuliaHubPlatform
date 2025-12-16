
struct MeasuredVariable end
Symbolics.option_to_metadata_type(::Val{:measured}) = MeasuredVariable
ismeasured(x::Num, args...) = ismeasured(Symbolics.unwrap(x), args...)
function ismeasured(x, default = false)
    p = Symbolics.getparent(x, nothing)
    p === nothing || (x = p)
    Symbolics.getmetadata(x, MeasuredVariable, default)
end

function setmeasured(x)
    setmetadata(x, MeasuredVariable, true)
end

function measured_values(sys, v=all_variable_symbols(sys))
    filter(x -> ismeasured(x, false), v)
end

function measured_system_values(sys)
    reference_container = symbolic_container(sys)
    return measured_values(reference_container)
end

function measured_names(measured)
    return string.(measured)
end

@mtkmodel MeasureComponent begin 
    @variables begin 
        value(t), [measured = true]
    end
end

function Measurement(sensor; name)
    @assert length(variable_symbols(sensor)) == 1 "The Measurement helper requires that the measurement component have only one scalar-valued state"
    @variables t value(t) [measured = true]
    return ODESystem([
        value ~ first(sensor.states)
    ], t; name = name)
end

export Measurement, MeasureComponent

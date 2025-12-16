module Hydration
using Dates
using REPL

include("timing_utils.jl")
include("water.jl")

function __init__()
    # Stolen from https://github.com/timholy/Revise.jl/blob/9339375a179ea51371eb4625eb346795f6f8dc16/src/packagedef.jl#LL1332C13-L1350C16
    pushfirst!(REPL.repl_ast_transforms, hydration_first)
    if isdefined(Base, :active_repl_backend)
        push!(Base.active_repl_backend.ast_transforms, hydration_first)
    else
        # wait for active_repl_backend to exist
        t = @async begin
            iter = 0
            while !isdefined(Base, :active_repl_backend) && iter < 20
                sleep(0.05)
                iter += 1
            end
            if isdefined(Base, :active_repl_backend)
                push!(Base.active_repl_backend.ast_transforms, hydration_first)
            end
        end
        isdefined(Base, :errormonitor) && Base.errormonitor(t)
    end
end

# solen from: https://github.com/timholy/Revise.jl/blob/9339375a179ea51371eb4625eb346795f6f8dc16/src/packagedef.jl#L1222-L1235
# `hydration_first` gets called by the REPL prior to executing the next command (by having been pushed
# onto the `ast_transform` list).
# This uses invokelatest not for reasons of world age but to ensure that the call is made at runtime.
# This allows `Hydration_first` to be compiled without compiling `hydration` itself, and greatly
# reduces the overhead of using Hydration.
function hydration_first(ex)
    # call `hydration` first before executing the expression
    return Expr(:toplevel, :(Base.invokelatest($hydration)), ex)
end

function hydration()
    # Add stuff here for hydration to run it before each REPL statemement
    remember_water(stderr, Hour(1))
end

end

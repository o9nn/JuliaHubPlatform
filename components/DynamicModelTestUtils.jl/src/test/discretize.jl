_symbolic_subset(a, b) = all(av->any(bv->isequal(av, bv), b), a) 

function namespace_symbol(prefix, sym)
    return Symbol(string(prefix) * "/" * string(sym))
end

function make_cols(names, rows)
    cols = []
    for (i, name) in enumerate(names)
        col = zeros(length(rows))
        for (j, row) in enumerate(rows)
            col[j] = row[i]
        end
        push!(cols, name=>col)
    end
    return cols
end

"""
    discretize_solution(solution::SciMLBase.AbstractTimeseriesSolution[, time_ref::SciMLBase.AbstractTimeseriesSolution]; measured=nothing, all_observed=false)

`discretize_solution` takes a solution and an optional time reference and converts it into a dataframe. This dataframe will contain either:
* The variables (unknowns or observeds) named in `measured`, if provided.
* The variables marked as `measured` if `all_observed` is `false` and `measured` is `nothing`.
* All variables in the system if `all_observed` is `true` and `measured` is `nothing`.
The dataframe will contain a column called `timestamp` for each of the discretization times and a column for each observed value.

If no time reference is provided then the timebase used to discretize `solution` will be used instead. 
"""
function discretize_solution(solution::SciMLBase.AbstractTimeseriesSolution, time_ref::SciMLBase.AbstractTimeseriesSolution; measured=nothing, all_observed=false)
    container = SymbolicIndexingInterface.symbolic_container(time_ref)
    ref_t_vars = independent_variable_symbols(container)
    if length(ref_t_vars) > 1
        @error "PDE solutions not currently supported; only one iv is allowed"
    end
    return discretize_solution(solution, time_ref[first(ref_t_vars)]; measured=measured, all_observed=all_observed)
end
function discretize_solution(solution::SciMLBase.AbstractTimeseriesSolution, time_ref::DataFrame; measured=nothing, all_observed=false)
    @assert "timestamp" ∈ names(time_ref) "The dataset B must contain a column named `timestamp`"
    return discretize_solution(solution, collect(time_ref[!, "timestamp"]); measured=measured, all_observed=all_observed)
end
function discretize_solution(solution::SciMLBase.AbstractTimeseriesSolution; measured=nothing, all_observed=false)
    container = SymbolicIndexingInterface.symbolic_container(solution)
    ref_t_vars = independent_variable_symbols(container)
    if length(ref_t_vars) > 1
        @error "PDE solutions not currently supported; only one iv is allowed"
    end
    return discretize_solution(solution, solution[first(ref_t_vars)]; measured=measured, all_observed=all_observed )
end
function discretize_solution(solution::SciMLBase.AbstractTimeseriesSolution, time_ref::AbstractArray; measured=nothing, all_observed=false)
    container = SymbolicIndexingInterface.symbolic_container(solution)
    if isnothing(measured)
        if all_observed
            measured = all_variable_symbols(container)
        else
            measured = measured_values(container)
        end
    end
    ref_t_vars = independent_variable_symbols(container)
    if length(ref_t_vars) > 1
        @error "PDE solutions not currently supported; only one iv is allowed"
    end
    ref_t_var = first(ref_t_vars)
    
    matching_timebase = solution[ref_t_var] == time_ref

    if matching_timebase # if the time_ref match the timebase of the problem then use the value at the nodes regardless of if it's dense or sparse
        cols = make_cols(String["timestamp"; measured_names(measured)], solution[[ref_t_var; measured]])
    elseif solution.dense # continious-time solution, use the interpolant    
        cols = make_cols(String["timestamp"; measured_names(measured)], solution(time_ref, idxs=[ref_t_var; measured]))
    else
        throw("Cannot discretize_solution sparse solution about a different timebase.")
    end
    return DataFrame(cols)
end
export discretize_solution

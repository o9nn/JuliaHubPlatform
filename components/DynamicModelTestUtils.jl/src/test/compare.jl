"""
`DefaultComparison` is a convenience default comparison method for solutions and reference data. 

By default it's instantiated with l∞, l1, l2, RMS, and final l1 comparsons. More can be added by
passing a dict of name=>comparison function pairs into `field_cmp`, with the signature (delta, dt_delta, t) -> [vector difference].
The arguments of these comparison functions are `delta` for the raw difference beteween the value, `dt_delta` for the 
product of dt and delta (if you want to approximate the integral of the error), and the timestamps at which the data was produced.
The arguments consist of a vector per compared field.
"""
struct DefaultComparison
    field_cmp::Dict{Symbol, Function}
    function DefaultComparison(field_cmp::Dict{Symbol, Function}=Dict{Symbol, Function}(); use_defaults=true) 
        if use_defaults
            merge!(field_cmp, Dict{Symbol, Function}([
                :L∞ => (delta, dt_delta, t) -> norm.(dt_delta, Inf), 
                :L1 => (delta, dt_delta, t) -> norm.(dt_delta, 1), 
                :L2 => (delta, dt_delta, t) -> norm.(dt_delta, 2), 
                :rms => (delta, dt_delta, t) -> sqrt.(1/length(t) .* sum.(map(d-> d .^ 2, dt_delta))),
                :final => (delta, dt_delta, t) -> last.(delta)]))
        end
        return new(field_cmp)
    end
end

"""
    (d::DefaultComparison)(c, names, b, t, n, r) 

Default comparison implementation. Returns a dataframe that describes the result of running each comparison operation on the given data.

#Arguments

* `c`: The symbolic container from which the values we're comparing came
* `names`: The names to output for the fields we're comparing (not always the same as the names of the fields being compared)
* `b`: The symbolic variables that are being compared
* `t`: The timestamps at which the comparison is taking place 
* `n`: The new values being compared
* `r`: The reference values to compare against
"""
function (d::DefaultComparison)(c, names, b, t, n, r) 
    delta = map((o, re) -> o .- re, n, r)
    dt_delta = map(d -> 0.5 * (d[1:end-1] .+ d[2:end]) .* (t[2:end] .- t[1:end-1]), delta) 
    cmps = [name => cmper(delta, dt_delta, t) for (name, cmper) in d.field_cmp]
    return DataFrame([:var => names, cmps...,
        :observed => SymbolicIndexingInterface.is_observed.(c, b)])
end

function default_near_zero(result; ϵ=1e-7)
    obses = stack(result, 2:ncol(result)-1)
    for obs in eachrow(obses)
        @test obs[:value] < ϵ || "$(obs[:variable])($(obs[:var])) = $(obs[:value]) > ϵ = $ϵ"
    end
    @show result
end


"""
    compare(
        new_sol::SciMLBase.AbstractTimeseriesSolution,
        reference::DataFrame,
        over::Vector{<:Union{Pair{<:Any, String}, Pair{<:Any, Pair{String, String}}}}
        cmp=DefaultComparison(); warn_observed=true)

Lower level version of `compare` that lets you specify the mapping from values in `new_sol` (specified as either variable => name or variable => (input_name, output_name) pairs) to
the column names in `reference`. The tuple-result version lets you specify what the column names are when passed to the comparison method. Since the mapping is explicit
this version does not take `to_name`, but it still does take `warn_observed`. See the implicit-comparison version of `compare` for more information.
"""
function compare(
    new_sol::SciMLBase.AbstractTimeseriesSolution, 
    reference::DataFrame,
    over::Vector{<:Union{Pair{<:Any, String}, Pair{<:Any, Pair{String, String}}}},
    cmp=DefaultComparison(); warn_observed=true)
    basis = first.(over)
    reference_basis = map(e -> e isa Pair ? first(e) : e, last.(over))
    output_names = map((n1, n2) -> n2 isa Pair ? last(n2) : string(n1), basis, last.(over))
    @assert "timestamp" ∈ names(reference) "The dataset must contain a column named `timestamp`"
    new_container = SymbolicIndexingInterface.symbolic_container(new_sol)
    @assert all(SymbolicIndexingInterface.is_observed.((new_container, ), basis) .| SymbolicIndexingInterface.is_variable.((new_container, ), basis)) "All basis symbols must be observed in the new system"
    @assert all(b ∈ names(reference) for b in reference_basis) "The reference basis must be a subset of the columns in the reference data"
    if new_sol.dense
        dat = new_sol(reference[:, :timestamp], idxs=basis)
        obs = [[dat[i, j] for j=1:nrow(reference)] for i in eachindex(basis)]
        ref = collect.(eachcol(reference[:, Not(:timestamp)]))
        cmp(new_container, output_names, basis, reference[:, :timestamp], obs, ref)
    else 
        foldl(red, cmp(row[:timestamp], new_sol[row[:timestamp], basis], row[reference_basis]) for row in eachrow(reference); init=isnothing(init) ? nothing : init(basis))
    end
end

"""
    compare(
        new_sol::SciMLBase.AbstractTimeseriesSolution,
        reference::DataFrame,
        cmp=DefaultComparison(); to_name=string, warn_observed=true)

`compare` compares the results of `new_sol` to a result in `reference`, which may come from either experiment or a previous model execution.
The format of `reference` is that produced by [`discretize_solution`](@ref):

* A column named "timestamp" that indicates the time the measurement was taken (referenced to the same timebase as the solution provided)
* Columns with names matching the names in the completed system `new_sol` (fully qualified)

If `new_sol` is dense then the discretization nodes don't need to line up with the `reference` and the interpolant will be used instead.
If it is not dense then the user must ensure that the saved states in are at the same times as the "timestamp"s are in the reference data.

`compare` will use the comparison method specified by `cmp` to compare the two solutions; by default it uses the [`DefaultComparison`](@ref)
(which offers l1, l2, and rms comparisons per observed value), but you can pass your own. Look at `DefaultComparison` for more details on how.

The two optional named parameters are `to_name` (which is used to convert the symbolic variables in `new_sol` into valid column names for 
the dataframe) and `warn_observed` which controls whether `compare` complains about comparing observed values to non-observed values. We suggest
leaving `warn_observed` on for regression testing (it'll give you a note when MTK changes the simplifcation of the system).
"""
function compare(
    new_sol::SciMLBase.AbstractTimeseriesSolution,
    reference::DataFrame,
    cmp=DefaultComparison(); to_name=string, warn_observed=true)
    @assert "timestamp" ∈ names(reference) "The dataset must contain a column named `timestamp`"
    eval_syms = reference_syms(SymbolicIndexingInterface.symbolic_container(new_sol), reference, to_name, warn_observed)
    return compare(new_sol, reference, eval_syms .=> setdiff(names(reference), ["timestamp"]), cmp)
end

function reference_syms(new_container, reference, to_name, warn_observed)
    all_available = SymbolicIndexingInterface.all_variable_symbols(new_container)
    all_available_syms = Dict(to_name.(all_available) .=> all_available)
    return [ begin
        @assert ref_name ∈ keys(all_available_syms) "Reference value $ref_name not found in the new solution (either as state or observed)"
        matching_sym = all_available_syms[ref_name]
        if warn_observed && SymbolicIndexingInterface.is_observed(new_container, matching_sym)
            @warn "The variable $matching_sym is observed in the new solution while the past $ref_name is provided explicitly; problem structure may have changed"
        end
        matching_sym 
    end for ref_name in setdiff(names(reference), ["timestamp"])] 
end

export compare
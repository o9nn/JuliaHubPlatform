module NDSparseIO

using CSV
using PooledArrays
using NullableArrays
using NDSparseData
using WeakRefStrings
using DataFrames
using NamedTuples

export csv2ndsparse

import NDSparseData: cmpelts, copyelt!, NDSparse

const STR = WeakRefString{UInt8}

# NDSparse optimizations for PooledArrays
@inline cmpelts(a::PooledArray, i, j) = (x=cmp(a.refs[i],a.refs[j]); x)
@inline copyelt!(a::PooledArray, i, j) = (a.refs[i] = a.refs[j])

process_col(a::NullableArray{STR}) = PooledArray{String}(a.values)
process_col(a::NullableArray) = a.values
process_col(a) = a

function csv2ndsparse(f; named::Bool=false, idxcols=nothing, datacol=nothing, datacols=nothing, kw...)
    return NDSparse(CSV.read(f; kw...), named=named, idxcols=idxcols, datacol=datacol, datacols=datacols)
end

function NDSparse(df::DataFrame; named::Bool=false, idxcols=nothing, datacol=nothing, datacols=nothing, kw...)
    cols = map(process_col, df.columns)
    n = length(cols)
    if datacols === nothing
        if datacol === nothing
            datacols = [n]
        elseif isa(datacol, Symbol)
            datacols = [df.colindex.lookup[datacol]]
        else
            datacols = [datacol]
        end
    elseif any(x->isa(x,Symbol), datacols)
        datacols = map(x->df.colindex.lookup[x], datacols)
    end
    if idxcols === nothing
        idxcols = sort(setdiff([1:n;], datacols))
    elseif any(x->isa(x,Symbol), idxcols)
        idxcols = map(x->df.colindex.lookup[x], idxcols)
    end
    if length(datacols) == 1
        data = cols[datacols[1]]
    end
    if named
        idx = Columns(cols[idxcols]..., names=df.colindex.names[idxcols])
        if length(datacols) > 1
            data = Columns(cols[datacols]..., names=df.colindex.names[datacols])
        end
    else
        idx = Columns(cols[idxcols]...)
        if length(datacols) > 1
            data = Columns(cols[datacols]...)
        end
    end
    NDSparse(idx, data)
end

end

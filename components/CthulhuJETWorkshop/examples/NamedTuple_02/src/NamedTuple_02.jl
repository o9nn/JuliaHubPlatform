module NamedTuple_02

using JSON
using LinearAlgebra: norm2

function get_model(; include_metadata::Bool)
    # load data from file
    data = JSON.parsefile(joinpath(dirname(@__FILE__), "data.json"))

    # create a NamedTuple from the model data
    model = (;
        (
           Symbol(k) => v
           for (k,v) in pairs(data)
        )...
    )

    # (this should create a type-instability that will make `model.p0`
    #  type-unstable below - if not, just add more branches)
    if include_metadata
        return (; creation_time = time(), model...)
    end
    return model
end

function @main(ARGS)
    # obtain a model that should be run
    model = get_model(; include_metadata = rand(Bool))

    # construct a random input
    input = randn(3)

    # compute a 'result'
    result = norm2(input .- model.p0)

    # print it to the user
    println(Core.stdout, result)
end

# To solve:
#  1. Move the de-serialization to top-level (if tolerated)
#     - alternatively, switch to TOML and type-assert each result
#  2. Make the NamedTuples we return the same / type-stable
#     - altenatively, create an explicit `struct` type and return that

end # module NamedTuple_02

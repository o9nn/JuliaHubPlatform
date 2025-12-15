using Hydration
using Test
using Dates

@testset "Hydration.jl" begin 
    @testset "$file" for file in (
        "timing_utils.jl", 
        "water.jl",
    )
        include(file)
    end
end

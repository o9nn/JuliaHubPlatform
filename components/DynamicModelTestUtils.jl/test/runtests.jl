#using JuliaSimModelOptimizer
using DynamicModelTestUtils
using DifferentialEquations, DataFrames, ModelingToolkit
import SymbolicIndexingInterface
using Test

@testset "DynamicModelTestUtils.jl" begin
    #include("timeseries.jl")
    include("block_modeling.jl")
    include("instantaneous.jl")
    include("deployment.jl")
end

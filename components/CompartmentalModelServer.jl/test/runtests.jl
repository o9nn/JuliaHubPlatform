using Test
using OrdinaryDiffEq: ODEProblem
using CompartmentalModelServer: greet, problem, solution, simulate, ModelSolution, DEFAULTS

@testset "CompartmentalModelServer" begin
    @testset "greet" begin @test greet() == "Hello World!" end
    @testset "problem" begin @test problem() isa ODEProblem end
    @testset "solution" begin @test solution() isa ModelSolution end
    @testset "simulate" begin
        @test simulate() isa ModelSolution
        @test simulate(; Snew = 888.0::Float64, βnew = 0.7::Float64) isa ModelSolution
        @test simulate().time[end] == DEFAULTS.tstop
        @test simulate(; Snew = 345).S[1] == 345
        @test simulate(; Snew = 345, tstop = 10.0).time[end] == 10.0
    end
end

@testset "remember_water" begin
    iob = IOBuffer()
    period = Millisecond(500)
    sleep(1)
    Hydration.remember_water(iob, period)
    sleep(1)
    Hydration.remember_water(iob, period)
    Hydration.remember_water(iob, period)
    Hydration.remember_water(iob, period)
    sleep(1)
    Hydration.remember_water(iob, period)
    output = String(take!(iob))
    @test length(split(output, "\n"; keepempty=false)) == 3
end
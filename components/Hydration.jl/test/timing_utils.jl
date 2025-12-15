

@testset "is_it_time" begin
    prev_ref = Ref(now())
    @test Hydration.is_it_time!(prev_ref, Second(1)) == false
    sleep(1.1)
    @test Hydration.is_it_time!(prev_ref, Second(1)) == true
    @test Hydration.is_it_time!(prev_ref, Second(1)) == false
end
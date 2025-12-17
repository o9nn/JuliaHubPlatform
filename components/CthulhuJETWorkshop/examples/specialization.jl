@noinline transformed_random_sum(f::Function) = sum(f, randn(10))

function @main(ARGS::Vector{String})
	
    # compute a random norm
    rand_norm = transformed_random_sum(abs2)

    # print it to the user
    println(Core.stdout, rand_norm)

    return 0
end

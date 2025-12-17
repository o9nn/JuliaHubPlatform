using Cthulhu
using InteractiveUtils

function foo()
    true_or_false = rand(Bool)
    if true_or_false
        return 1
    else
        return 1.0
    end
end

@code_warntype foo()


# Pretty fun example since the branches are equivalent when passing a Float64
function sum_one_to(x)
    true_or_false = rand(Bool)
    if true_or_false
        r = 1:x
    else
        r = range(1, x, step = 1)
    end
    return sum(float, r)
end

@code_warntype sum_one_to(20.0)
@code_warntype sum_one_to(20)


# Selecting the method to descend into
@descend sum_one_to(20)


# Hidden type instability
function many_sums(x::Int)
    r = sum_one_to(float(x))
    r += sum_one_to(2x)
    r += sum_one_to(3x)
    return r
end

@code_warntype optimize = false many_sums(1)
@descend many_sums(1)


# Misc examples
numbers = Real[]
@code_warntype sum(numbers)

numbers = Union{Float64, Int}[]
@code_warntype sum(numbers)

numbers = Real[]
@code_warntype sum(float, numbers)

numbers = Union{Float64, Int}[]
@code_warntype sum(float, numbers)

# # Code introspection in Julia

# Load packages
using InteractiveUtils # implicitly loaded in the Julia REPL
using JuliaSyntax
using Cthulhu

# Define a function (i.e. the source code)
function plus(x, y)
    z = x + y
    return z
end

# Test it with some test data
x, y = 1, 2
plus(x, y)


# ## Parsing

code_string = """
function plus(x, y)
    z = x + y
    return z
end
"""

# The old way: Meta.parse
expr = Meta.parse(code_string)
dump(expr)

# The new way: JuliaSyntax.parse (JuliaSyntax is used by default since Julia version 1.10)
tree = JuliaSyntax.parse(JuliaSyntax.GreenNode, code_string)

# Show the tree together with the source code string
show(stdout, MIME("text/plain"), tree, code_string)


# ## Lowering

@code_lowered plus(x, y)


# ## Julia IR, without and with optimizations

@code_typed optimize = false plus(x, y)
@code_typed optimize = true plus(x, y)

@code_warntype optimize = false plus(x, y)
@code_warntype optimize = true plus(x, y)


# ## LLVM IR, without and with optimizations

@code_llvm optimize = false debuginfo = :none plus(x, y)
@code_llvm optimize = true debuginfo = :none plus(x, y)


# ## Native CPU instructions (output from LLVM)

@code_native debuginfo = :none plus(x, y)


# ## Other input types

x, y = 1, 2.0 # Int64, Float64

@code_typed optimize = false plus(x, y)
@code_typed optimize = true plus(x, y)

@code_warntype optimize = false plus(x, y)
@code_warntype optimize = true plus(x, y)


x, y = [1, 2, 3], [4, 5, 6] # Vector{Int}, Vector{Int}

@code_typed optimize = false plus(x, y)
@code_typed optimize = true plus(x, y)

@code_warntype optimize = false plus(x, y)
@code_warntype optimize = true plus(x, y)

@code_llvm optimize = false debuginfo = :none plus(x, y)
@code_llvm optimize = true debuginfo = :none plus(x, y)

@code_native debuginfo = :none plus(x, y)

x, y = [1, 2, 3], 4 # Vector{Int}, Int
@code_typed optimize = false plus(x, y)
@code_typed optimize = true plus(x, y)


# ## Some more complicated examples

# Function with a branch
function f1(x)
    y = sin(x)
    if x > 5.0
        y = y + cos(x)
    end
    return exp(2) + y
end

x = 6.0
# @code_typed optimize = false f1(x)
# @code_typed optimize = true f1(x)
@code_warntype optimize = false f1(x)
@code_warntype optimize = true f1(x)


# A function where the type of a variable depends on a conditional
function f1(x)
    y = 123.0
    if x > 5.0
        y = 123
    end
    return exp(2) + y
end

x = 7.0
# @code_typed optimize = false f1(x)
# @code_typed optimize = true f1(x)
@code_warntype optimize = false f1(x)
@code_warntype optimize = true f1(x)


# ## Cthulhu: recursive introspection

function plus(x, y)
    z = x + y
    return z
end

x, y = 6, 7
@descend plus(x, y)


@descend f1(x)

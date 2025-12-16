# Cthulu.jl + JET.jl workshop

This repository contains some examples and code snippets. Tested and developed
with Julia version 1.12.0-rc3 and:

```
Cthulhu@v2.17.8
JET@v0.10.7
JuliaSyntax@v1.0.2
```


## Common issues related to `--trim`

Specific code issues:
  1. `(vec...,)`
     - this operation requires an awkward `for`-loop code to `--trim`
     - often causes downstream inference issues due to `Tuple{Vararg{...}}`
  2. `[vec_a..., vec_b...]`
     - this is "Vector{Int}" all the way down, but poor `apply_iterate` support breaks this
       (takeaway: only ever splat Tuples, not Vectors)
     - https://github.com/JuliaLang/julia/issues/57830
  3. `return (; attributes...)`
     - very easy for this to become type-unstable when considering multiple `return` values
     - best to explicitly write the `@NamedTuple{...}` type you want to return and use that (similar to an anonymous struct)
  4. `eval(...)` (CPUSummary.jl / HostCPUFeatures.jl are the notable examples in the community)
     - CPUSummary.jl has been fixed already, HostCPUFeatures.jl has not

Common patterns that cause issues:
  1. Returning different types based on a runtime value
     - c.f. https://github.com/SciML/OrdinaryDiffEq.jl/issues/2855
  2. De-serializing data
     - if it can be done at top-level, this is a "compile-time" operation and (mostly) trimmable for free!
     - be careful about using heavy de-serialization libraries, since the binaries will ship with your `--trim` code
  3. Manual specialization required
     - `foo(f::F) where {F <: Function}` (versus `foo(f::Function)`)
       - https://github.com/JuliaLang/julia/pull/57660/
     - `foo(::Type{T}) where T` (versus `foo(T)`)
       - https://github.com/JuliaLang/LinearAlgebra.jl/pull/1419
  4. (Advanced) Manual de-specialization required
     - a function that only constructs an object (w/ no type-parameters)
       - https://github.com/JuliaLang/julia/blob/c4683c41a8f36123de18a0d68092f23e2427f55c/contrib/juliac/juliac-trim-base.jl#L6
     - a function that constructs a type and then compares it using other polymorphic operations (e.g. `===` / `!==` / `isa`)
       - https://github.com/JuliaLang/julia/pull/56120/

Good patterns:
  1. Returning a `Const(...)` value (Tuple / Integer / Symbol / Type) based on a value's type
     - this is the "Tim Holy Trait trick"
  2. Make all struct fields "concretely-typed" (i.e. parameterized or a specific concrete type)

Good "natural-feeling" examples:
   - TOML stdlib, before `--trim` fixes (https://github.com/JuliaLang/julia/pull/55016/)

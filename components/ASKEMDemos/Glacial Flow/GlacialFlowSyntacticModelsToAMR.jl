# Based on ice flow model example from Decapodes.jl (https://algebraicjulia.github.io/Decapodes.jl/dev/ice_dynamics/#Composing-models) 
# and SyntacticModels.jl example (https://algebraicjulia.github.io/SyntacticModels.jl/dev/generated/composite_models_examples/)

using SyntacticModels

using SyntacticModels.AMR
using SyntacticModels.ASKEMDecapodes
using SyntacticModels.ASKEMUWDs
using SyntacticModels.Composites

using MLStyle
import SyntacticModels.ASKEMDecapodes.Decapodes as Decapodes
using Catlab
using Catlab.RelationalPrograms
using Catlab.WiringDiagrams
using Test
using JSON3
using Decapodes
using GraphViz
draw(uwd) = to_graphviz(uwd, box_labels=:name, junction_labels=:variable)


h = Typed(:h, :Form0)
Γ = Typed(:Γ, :Form1)
n = Typed(:n, :Constant)

variables = [h, Γ, n]

c = [n, Γ]
s = [Statement(:dynamics, [Γ, n]), Statement(:stress, [Γ,n])]
u = ASKEMUWDs.UWDExpr(c,s)

u_tables = ASKEMUWDs.construct(RelationDiagram, u)
draw(u_tables)

h1 = AMR.Header("glacial_flow",
  "modelreps.io/DecaExpr",
  "Glacial flow",
  "DecaExpr",
  "v1.0")


halfar_eq2 = Decapodes.parse_decapode(quote
  h::Form0
  Γ::Form1
  n::Constant

  ḣ == ∂ₜ(h)
  ḣ == ∘(⋆, d, ⋆)(Γ * d(h) * avg₀₁(mag(♯(d(h)))^(n-1)) * avg₀₁(h^(n+2)))
end
)

d1 = ASKEMDecaExpr(h1,halfar_eq2)

h2 = AMR.Header("glens_law","modelreps.io/DecaExpr",
"Glens Law",
"DecaExpr",
"v1.0")

glens_law = Decapodes.parse_decapode(quote
    Γ::Form1
    (A,ρ,g,n)::Constant

    Γ == (2/(n+2))*A*(ρ*g)^n
end
)

d2 = ASKEMDecaExpr(h2,glens_law)

hc = AMR.Header("composite_physics", "modelreps.io/Composite", "A composite model", "CompositeModelExpr", "v0.0")
m = CompositeModelExpr(hc, u, [OpenModel(d1, [:Γ, :n]), OpenModel(d2, [:Γ, :n])])

JSON3.pretty(m)



composite = oapply(m)
draw(apex(composite))
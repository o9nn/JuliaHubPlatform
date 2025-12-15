using Hardened
using Documenter

DocMeta.setdocmeta!(Hardened, :DocTestSetup, :(using Hardened); recursive=true)

makedocs(;
    modules=[Hardened],
    authors="Juliahub, Inc.",
    repo="https://github.com/juliacomputing/Hardened.jl/blob/{commit}{path}#{line}",
    sitename="Hardened.jl",
    format=Documenter.HTML(;
        prettyurls=get(ENV, "CI", "false") == "true",
        canonical="https://juliacomputing.github.io/Hardened.jl",
        assets=String[],
    ),
    pages=[
        "Home" => "index.md",
    ],
)

deploydocs(;
    repo="github.com/juliacomputing/Hardened.jl",
)

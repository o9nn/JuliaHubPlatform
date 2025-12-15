using DynamicModelTestUtils
using Documenter

DocMeta.setdocmeta!(DynamicModelTestUtils, :DocTestSetup, :(using DynamicModelTestUtils); recursive=true)

makedocs(;
    modules=[DynamicModelTestUtils],
    authors="Ben Chung <benjamin.chung@juliahub.com> and contributors",
    repo="https://github.com/BenChung/DynamicModelTestUtils.jl/blob/{commit}{path}#{line}",
    sitename="DynamicModelTestUtils.jl",
    format=Documenter.HTML(;
        prettyurls=get(ENV, "CI", "false") == "true",
        edit_link="master",
        assets=String[],
    ),
    pages=[
        "Home" => "index.md",
    ],
)
deploydocs(
    repo = "github.com/JuliaComputing/DynamicModelTestUtils.jl.git",
)
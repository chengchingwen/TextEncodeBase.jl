using TextEncodeBase
using Documenter

DocMeta.setdocmeta!(TextEncodeBase, :DocTestSetup, :(using TextEncodeBase); recursive=true)

makedocs(;
    modules=[TextEncodeBase],
    authors="chengchingwen <adgjl5645@hotmail.com> and contributors",
    repo="https://github.com/chengchingwen/TextEncodeBase.jl/blob/{commit}{path}#{line}",
    sitename="TextEncodeBase.jl",
    format=Documenter.HTML(;
        prettyurls=get(ENV, "CI", "false") == "true",
        canonical="https://chengchingwen.github.io/TextEncodeBase.jl",
        assets=String[],
    ),
    pages=[
        "Home" => [
            "index.md",
            "design.md",
            "api.md",
        ],
    ],
)

deploydocs(;
    repo="github.com/chengchingwen/TextEncodeBase.jl",
    devbranch="main",
)

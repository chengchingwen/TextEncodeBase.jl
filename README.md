# TextEncodeBase

[![Stable](https://img.shields.io/badge/docs-stable-blue.svg)](https://chengchingwen.github.io/TextEncodeBase.jl/stable)
[![Dev](https://img.shields.io/badge/docs-dev-blue.svg)](https://chengchingwen.github.io/TextEncodeBase.jl/dev)
[![Build Status](https://github.com/chengchingwen/TextEncodeBase.jl/actions/workflows/CI.yml/badge.svg?branch=main)](https://github.com/chengchingwen/TextEncodeBase.jl/actions/workflows/CI.yml?query=branch%3Amain)
[![Coverage](https://codecov.io/gh/chengchingwen/TextEncodeBase.jl/branch/main/graph/badge.svg)](https://codecov.io/gh/chengchingwen/TextEncodeBase.jl)


An api for encoding text, built on top of [WordTokenizers.jl](https://github.com/JuliaText/WordTokenizers.jl).
 Providing a framework to easily define custom methods to convert strings into indices.


# Usages

Here are some explanation and examples for using `TextEncodeBase.jl`, you can also find other information
 from the [docs](https://chengchingwen.github.io/TextEncodeBase.jl/dev) or [test](/test/runtests.jl)

## Vocabulary

The vocabulary part contains only two api, the `Vocab` struct and the `lookup` function.
 The `lookup` function is bidirectional (convert string to indices and back).

```julia
julia> vocab = Vocab(["a", "b", "c", "a", "b", "c"])
Vocab{String, StaticArrays.SizedVector{3, String, Vector{String}}}(size = 3, unk = [UNK], unki = 0)

julia> vocab_unk = Vocab(["a", "b", "xxx"], "xxx")
Vocab{String, StaticArrays.SizedVector{3, String, Vector{String}}}(size = 3, unk = xxx, unki = 3)

julia> lookup(vocab, "b")
2

julia> lookup(vocab, "d")
0

julia> lookup(vocab_unk, "d")
3

julia> lookup(vocab, 1)
"a"

julia> lookup(vocab, 10000)
"[UNK]"

julia> lookup(vocab_unk, 10000)
"xxx"

julia> lookup(vocab, ["b", "c", "a", "A", "[UNK]"])
5-element Vector{Int64}:
 2
 3
 1
 0
 0

julia> lookup(OneHot, vocab, "a")
3-element OneHot{3}:
 1
 0
 0

julia> lookup(OneHot, vocab, 3)
ERROR: DomainError with c:
cannot convert `lookup(::Vocab, 3)` = "c" into one-hot representation.
Stacktrace:
[...]

julia> oha = lookup(OneHot, vocab, ["a" "b"; "c" "d"])
3x2x2 OneHotArray{3, 3, Matrix{OneHot{0x00000003}}}:
[:, :, 1] =
 1  0
 0  0
 0  1

[:, :, 2] =
 0  0
 1  0
 0  0

julia> lookup(vocab, oha)
2×2 Matrix{String}:
 "a"  "b"
 "c"  "[UNK]"

```

## Tokenizer

The tokenizer part is built ontop of `WordTokenizers.jl` and provide a high-level api
 to control/augment the tokenization. There're some differences between `WordTokenizers.jl`.
 `WordTokenizers.jl` provides a set of tokenizers and a low-level api (`TokenBuffer`) for define
 custom tokenizers. It's mainly focus on how to split a setnece into tokens. We, on the other hand,
 focus on how to combine different tokenizer or include other information during the tokenization.
 For example, sometimes you might want to prevent urls from being splited or add an extra tag to it,
 these can be done by defining a custom `AbstractTokenizer` and overload some methods. Besides, we
 force the user to explicit wrap the input as one of the stages (`Document`/`Sentence`/`Word`/...),
 so no confusion.

## TextEncoder

The text encoder is just a combination of vocabulary and tokenizer.

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
 For example, sometimes you might want to prevent urls from being splited or add some extra tags to it,
 these can be done by defining a custom `AbstractTokenizer` and overload some methods. Besides, we
 force the user to explicit wrap the input as one of the stages (`Document`/`Sentence`/`Word`/...),
 so no confusion.

### Example of using the Tokenizer api

Here is an example that wrapped the word tokenizer and wordpiece from `Transformers.jl` into our Tokenizer api.

```julia
using Transformers
using Transformers.Pretrain
using Transformers.BidirectionalEncoder: WordPiece, bert_cased_tokenizer

using TextEncodeBase
using TextEncodeBase: NestedTokenizer, BaseTokenization, Sentence, Word, SubWord, getvalue, Splittable

struct BertCasedTokenization <: BaseTokenization
    wordpiece::WordPiece
end

# split sentence with `bert_cased_tokenizer` (define with WordTokenizers.jl's `TokenBuffer`)
TextEncodeBase.splitting(::BertCasedTokenization, s::Sentence) = bert_cased_tokenizer(getvalue(s))

# word is splittable with WordPiece
TextEncodeBase.splittability(::BertCasedTokenization, w::Word) = Splittable()

# split word with `WordPiece`
TextEncodeBase.splitting(t::BertCasedTokenization, w::Word) = t.wordpiece(getvalue(w))

tokenizer = pretrain"bert-cased_L-12_H-768_A-12:tokenizer" # this is just `bert_cased_tokenizer`
wordpiece = pretrain"bert-cased_L-12_H-768_A-12:wordpiece"

tkr = NestedTokenizer(BertCasedTokenization(wordpiece))

text1 = "Peter Piper picked a peck of pickled peppers"
single_without_TEB = text1 |> tokenizer |> wordpiece
single_with_TEB = tkr(Sentence(text1))

# `NestedTokenizer` return vector of vector
@assert single_without_TEB == map(getvalue, single_with_TEB[])

julia> single_without_TEB
11-element Vector{String}:
 "Peter"
 "Piper"
 "picked"
 "a"
 "p"
 "##eck"
 "of"
 "pick"
 "##led"
 "pepper"
 "##s"

julia> single_with_TEB
1-element Vector{Vector{TextEncodeBase.TokenStage}}:
 [Token("Peter"), Token("Piper"), Token("picked"), Token("a"), Token("p"), Token("##eck"), Token("of"), Token("pick"), Token("##led"), Token("pepper"), Token("##s")]

julia> single_without_TEB == map(getvalue, single_with_TEB[])
true


# define stage for batch of data
struct BatchSentence{A<:AbstractVector, M} <: TextEncodeBase.DocumentStage
    x::A
    meta::M
end

BatchSentence(x) = BatchSentence(x, nothing)
TextEncodeBase.setmeta(x::BatchSentence, meta) = BatchSentence(x.x, meta)
TextEncodeBase.setvalue(x::BatchSentence, y) = BatchSentence(y, x.meta)

# splittability and split behavior for `BatchSentence`
TextEncodeBase.splittability(::BertCasedTokenization, ::BatchSentence) = Splittable()
TextEncodeBase.splitting(::BertCasedTokenization, s::BatchSentence) = s.x

text2 = "Fuzzy Wuzzy was a bear"
texts = [text1, text2]

batch_without_TEB = map(wordpiece∘tokenizer, texts)
batch_with_TEB = tkr(BatchSentence(texts))

@assert batch_without_TEB == TextEncodeBase.nestedcall(getvalue, batch_with_TEB)

julia> batch_without_TEB
2-element Vector{Vector{String}}:
 ["Peter", "Piper", "picked", "a", "p", "##eck", "of", "pick", "##led", "pepper", "##s"]
 ["Fu", "##zzy", "Wu", "##zzy", "was", "a", "bear"]

julia> batch_with_TEB
2-element Vector{Vector{TextEncodeBase.TokenStage}}:
 [Token("Peter"), Token("Piper"), Token("picked"), Token("a"), Token("p"), Token("##eck"), Token("of"), Token("pick"), Token("##led"), Token("pepper"), Token("##s")]
 [Token("Fu"), Token("##zzy"), Token("Wu"), Token("##zzy"), Token("was"), Token("a"), Token("bear")]

julia> batch_without_TEB == TextEncodeBase.nestedcall(getvalue, batch_with_TEB)
true

```

Since the wordpiece break word into subword, we might want to know which word each subword belongs to:

```julia
julia> itkr = NestedTokenizer(TextEncodeBase.IndexedTokenization(BertCasedTokenization(wordpiece)));

julia> ibatch_with_TEB = itkr(BatchSentence(texts));

# subword from same word having the same `word_id`
julia> ibatch_with_TEB[1]
11-element Vector{TextEncodeBase.TokenStage}:
 Token("Peter", (sentence_id = 1, word_id = 1, token_id = 1))
 Token("Piper", (sentence_id = 1, word_id = 2, token_id = 2))
 Token("picked", (sentence_id = 1, word_id = 3, token_id = 3))
 Token("a", (sentence_id = 1, word_id = 4, token_id = 4))
 Token("p", (sentence_id = 1, word_id = 5, token_id = 5))
 Token("##eck", (sentence_id = 1, word_id = 5, token_id = 6))
 Token("of", (sentence_id = 1, word_id = 6, token_id = 7))
 Token("pick", (sentence_id = 1, word_id = 7, token_id = 8))
 Token("##led", (sentence_id = 1, word_id = 7, token_id = 9))
 Token("pepper", (sentence_id = 1, word_id = 8, token_id = 10))
 Token("##s", (sentence_id = 1, word_id = 8, token_id = 11))

julia> ibatch_with_TEB[2]
7-element Vector{TextEncodeBase.TokenStage}:
 Token("Fu", (sentence_id = 2, word_id = 1, token_id = 1))
 Token("##zzy", (sentence_id = 2, word_id = 1, token_id = 2))
 Token("Wu", (sentence_id = 2, word_id = 2, token_id = 3))
 Token("##zzy", (sentence_id = 2, word_id = 2, token_id = 4))
 Token("was", (sentence_id = 2, word_id = 3, token_id = 5))
 Token("a", (sentence_id = 2, word_id = 4, token_id = 6))
 Token("bear", (sentence_id = 2, word_id = 5, token_id = 7))

```

## TextEncoder

The text encoder is just a combination of vocabulary and tokenizer.

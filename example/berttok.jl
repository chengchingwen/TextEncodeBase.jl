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

#=
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

=#


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

#=
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

=#


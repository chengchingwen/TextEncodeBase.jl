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
TextEncodeBase.@stage BatchSentence{A<:AbstractVector, M} TextEncodeBase.DocumentStage

# struct BatchSentence{A<:AbstractVector, M} <: TextEncodeBase.DocumentStage
#     x::A
#     meta::M
# end
# BatchSentence(x) = BatchSentence(x, nothing)
# TextEncodeBase.setmeta(x::BatchSentence, meta) = BatchSentence(x.x, meta)
# TextEncodeBase.setvalue(x::BatchSentence, y) = BatchSentence(y, x.meta)

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

using TextEncodeBase: IndexedTokenization

itkr = NestedTokenizer(IndexedTokenization(BertCasedTokenization(wordpiece)))

ibatch_with_TEB = itkr(BatchSentence(texts))

#=
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

=#


using TextEncodeBase: nestedcall, with_head_tail, trunc_and_pad, nested2batch

# construct `Vocab` with `WordPiece`
vocab = Vocab(wordpiece.vocab, wordpiece.vocab[wordpiece.unk_idx])

# define encoder with `TextEncoder`
enc = TextEncoder(
    itkr, vocab,
    nested2batch ∘ trunc_and_pad(nothing, vocab.unk) ∘ with_head_tail("[CLS]", "[SEP]") ∘ nestedcall(getvalue)
)

#=
julia> encode(enc, BatchSentence(texts))
28996x13x2 OneHotArray{28996, 3, Matrix{OneHot{0x00007144}}}:
[...]

julia> decode(enc, ans)
13×2 Matrix{String}:
 "[CLS]"   "[CLS]"
 "Peter"   "Fu"
 "Piper"   "##zzy"
 "picked"  "Wu"
 "a"       "##zzy"
 "p"       "was"
 "##eck"   "a"
 "of"      "bear"
 "pick"    "[SEP]"
 "##led"   "[UNK]"
 "pepper"  "[UNK]"
 "##s"     "[UNK]"
 "[SEP]"   "[UNK]"

=#

abstract type AbstractTextEncoder end

tokenize(e::AbstractTextEncoder, x)  = e.tokenizer(x)
process(e::AbstractTextEncoder, x) = map(getvalue, x)
lookup(e::AbstractTextEncoder, x) = lookup(OneHot, e.vocab, x)
encode(e::AbstractTextEncoder, x) = lookup(e, process(e, tokenize(e, x)))
decode(e::AbstractTextEncoder, x) = lookup(e.vocab, x)

struct TextEncoder{T<:AbstractTokenizer, V<:AbstractVocabulary, P} <: AbstractTextEncoder
    tokenizer::T
    vocab::V
    process::P
end

TextEncoder(tkr, vocab) = TextEncoder(tkr, vocab, Base.Fix1(map, getvalue))

process(e::TextEncoder, x) = e.process(x)

abstract type AbstractTextEncoder end

"""
    process(::AbstractTextEncoder)

Get processing function of given encoder.
"""
process(e::AbstractTextEncoder) = e.process
process(::Type{AbstractTextEncoder}) = nestedcall(getvalue)

"""
    tokenize(e::AbstractTextEncoder, x)

Use encoder's tokenizer to tokenize `x`.
"""
tokenize(e::AbstractTextEncoder, x)  = e.tokenizer(x)

"""
    process(e::AbstractTextEncoder, x)

Use encoder's processing function to process `x`.
"""
process(e::AbstractTextEncoder, x) = process(e)(x)

"""
    lookup(e::AbstractTextEncoder, x)

Lookup `x` in encoder's vocabulary.
"""
lookup(e::AbstractTextEncoder, x) = lookup(OneHot, e.vocab, x)

"""
    encode(e::AbstractTextEncoder, x)

Encode `x`.
"""
encode(e::AbstractTextEncoder, x) = lookup(e, process(e, tokenize(e, x)))

"""
    decode(e::AbstractTextEncoder, x)

Decode `x`.
"""
decode(e::AbstractTextEncoder, x) = lookup(eltype(e.vocab), e.vocab, x)

"""
    TextEncoder(tokenizer, vocab, process = nestedcall(getvalue))

A simple encoder implementation.
"""
struct TextEncoder{T<:AbstractTokenizer, V<:AbstractVocabulary, P} <: AbstractTextEncoder
    tokenizer::T
    vocab::V
    process::P
end

TextEncoder(tkr::AbstractTokenizer, vocab::AbstractVocabulary) = TextEncoder(tkr, vocab, process(AbstractTextEncoder))

TextEncoder(builder, tkr::AbstractTokenizer, vocab::AbstractVocabulary) = TextEncoder(builder, TextEncoder(tkr, vocab))

"""
    TextEncoder(builder, e::TextEncoder)

Given an encoder, return a new encoder that has the same tokenizer and vocabulary. `builder` is
 a function that take a encoder and return a new processing function.
"""
TextEncoder(builder, e::TextEncoder) = TextEncoder(e.tokenizer, e.vocab, builder(e))

using StructWalk

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
    onehot_encode(e::AbstractTextEncoder, x)

Lookup `x` in encoder's vocabulary. Return one-hot encoded vectors.
"""
onehot_encode(e::AbstractTextEncoder, x) = lookup(OneHot, e.vocab, x)

"""
    lookup(e::AbstractTextEncoder, x)

Lookup `x`. This is basically [`onehot_encode`](@ref) but can be overloaded for extra processing.
"""
lookup(e::AbstractTextEncoder, x) = onehot_encode(e, x)

"""
    encode_indices(e::AbstractTextEncoder, x)

Encode for indices. Encode `x` without calling `lookup` bound with `e`.
"""
encode_indices(e::AbstractTextEncoder, x) = process(e, tokenize(e, x))

"""
    encode(e::AbstractTextEncoder, x)

Encode `x`.
"""
encode(e::AbstractTextEncoder, x) = lookup(e, encode_indices(e, x))

"""
    decode_indices(e::AbstractTextEncoder, x)

Decode from indices. Decode `x` by reverse lookup `x` in `e.vocab`.
"""
decode_indices(e::AbstractTextEncoder, x) = lookup(eltype(e.vocab), e.vocab, x)

"""
    decode(e::AbstractTextEncoder, x)

Decode `x`. This is basically [`decode_indices`](@ref) but can be overloaded for post-processing.
"""
decode(e::AbstractTextEncoder, x) = decode_indices(e, x)

"""
    decode_text(e::AbstractTextEncoder, x)

Decode `x` into texts. This is basically [`join_text`](@ref) with [`decode`](@ref) but can be overloaded
 for post-processing.
"""
decode_text(e::AbstractTextEncoder, x) = join_text(decode(e, x))

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


StructWalk.children(::TokenizerStyle, x::AbstractTextEncoder) = StructWalk.children(WalkStyle, x)
StructWalk.iscontainer(::TokenizerStyle, x::AbstractTextEncoder) = false

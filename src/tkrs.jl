struct MixedTokenization{T <:Tuple{<:AbstractTokenization, Vararg{<:AbstractTokenization}}} <: AbstractTokenization
    ts::T
end
MixedTokenization(t, t2, ts...) = MixedTokenization((t, t2, ts...))

Base.getindex(t::MixedTokenization, i) = t.ts[i]

Base.@kwdef struct WordTokenization{S, T} <: BaseTokenization
    split_sentences::S = WordTokenizers.split_sentences
    tokenize::T = WordTokenizers.tokenize
end

@inline splitting(t::WordTokenization, d::DocumentStage)    = t.split_sentences(getvalue(d))
@inline splitting(t::WordTokenization, s::SentenceStage)    = t.tokenize(getvalue(s))
@inline splitting(t::WordTokenization, s::SubSentenceStage) = t.tokenize(getvalue(s))

"tokenizer that return flat array instead of nested array of tokens"
struct FlatTokenizer{T<:AbstractTokenization} <: AbstractTokenizer
    tokenization::T
end
FlatTokenizer() = FlatTokenizer(DefaultTokenization())

tokenization(tkr::FlatTokenizer) = tkr.tokenization

"tokenizer that return nested array instead of flat array of tokens"
struct NestedTokenizer{T<:AbstractTokenization} <: AbstractTokenizer
    tokenization::T
end
NestedTokenizer() = NestedTokenizer(DefaultTokenization())

tokenization(tkr::NestedTokenizer) = tkr.tokenization

@inline tokenize(tkr::NestedTokenizer, p::ParentStages, t::AbstractTokenization, x::DocumentStage) = tokenize_procedure!(push!, Vector{TokenStage}[], tkr, p, t, x)
@inline tokenize(tkr::NestedTokenizer, ::Nothing, t::AbstractTokenization, x::SentenceStage) = [tokenize_procedure(tkr, nothing, t, x)]


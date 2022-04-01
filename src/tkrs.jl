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

@inline tokenize(tkr::FlatTokenizer, s::ParentStages, t::AbstractTokenization, x::TokenStages) = tokenize_procedure(tkr, s, t, x)
@inline tokenize(tkr::FlatTokenizer, s::ParentStages, t::AbstractTokenization, x::TokenStage) = isempty(getvalue(x)) ? TokenStage[] : TokenStage[wrap(tkr, s, t, x)]

"tokenizer that return nested array instead of flat array of tokens"
struct NestedTokenizer{T<:AbstractTokenization} <: AbstractTokenizer
    tokenization::T
end
NestedTokenizer() = NestedTokenizer(DefaultTokenization())

tokenization(tkr::NestedTokenizer) = tkr.tokenization

@inline tokenize(tkr::NestedTokenizer, p::ParentStages, t::AbstractTokenization, x::TokenStages) = tokenize_procedure!(push!, Vector{Vector}[], tkr, p, t, x)
@inline tokenize(tkr::NestedTokenizer, p::ParentStages, t::AbstractTokenization, x::DocumentStage) = tokenize_procedure!(push!, Vector{TokenStage}[], tkr, p, t, x)
@inline tokenize(tkr::NestedTokenizer, p::ParentStages, t::AbstractTokenization, x::Union{SentenceStage, SubSentenceStage, WordStage, SubWordStage}) = tokenize_procedure!(append!, TokenStage[], tkr, p, t, x)
@inline tokenize(tkr::NestedTokenizer, ::Nothing, t::AbstractTokenization, x::SentenceStage) = [tokenize_procedure(tkr, nothing, t, x)]

@inline tokenize(tkr::NestedTokenizer, p::ParentStages, t::AbstractTokenization, x::TokenStage) = isempty(getvalue(x)) ? TokenStage[] : TokenStage[wrap(tkr, p, t, x)]

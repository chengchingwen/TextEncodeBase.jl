struct MixedTokenization{T <:Tuple{<:AbstractTokenization, Vararg{<:AbstractTokenization}}} <: AbstractTokenization
    ts::T
end
MixedTokenization(t, t2, ts...) = MixedTokenization((t, t2, ts...))

Base.getindex(t::MixedTokenization, i) = t.ts[i]

Base.@kwdef struct WordTokenization{S, T} <: AbstractTokenization
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

"tokenizer that run the default behavior"
struct NaiveTokenizer <: AbstractTokenizer end

"default behavior but counting the index"
struct NaiveIndexedTokenizer <: AbstractTokenizer end
tokenization(::NaiveIndexedTokenizer) = IndexedTokenization()

"default behavior but don't split some pattern"
struct NaiveMatchTokenizer <: AbstractTokenizer
    patterns::Vector{Regex}
end
tokenization(tkr::NaiveMatchTokenizer) = MatchTokenization(tkr.patterns)

"default behavior but counting index and don't split some pattern"
struct NaiveIndexedMatchTokenizer <: AbstractTokenizer
    patterns::Vector{Regex}
end
tokenization(tkr::NaiveIndexedMatchTokenizer) = IndexedTokenization(MatchTokenization(tkr.patterns))

# splitting(tkr::NaiveIndexedMatchTokenizer, ::IndexedTokenization, s::SentenceStage) = splitting(tkr, MatchTokenization(tkr.patterns), s)

# wrap(tkr::NaiveIndexedMatchTokenizer, t::IndexedTokenization, s::SentenceStage, (i, x)) = updatemeta(wrap(tkr, MatchTokenization(tkr.patterns), s, x), (offsets = i,))

# tokenize(tkr::NaiveIndexedMatchTokenizer, t::IndexedTokenization, ::Nothing, w::WordStage) = tokenize(tkr, MatchTokenization(tkr.patterns), nothing, w)

# splitting(tkr::NaiveIndexedMatchTokenizer, ::MatchTokenization, w::WordStage, x) = splitting(tkr, IndexedTokenization(), w, x)

# wrap(tkr::NaiveIndexedMatchTokenizer, t::MatchTokenization, w::WordStage, (i, x)) = updatemeta(wrap(t, w, x), (offsets = i,))

# function wrap(tkr::NaiveIndexedMatchTokenizer, t::IndexedTokenization{MatchTokenization}, w::TokenStage)
#     y = wrap(t, w)
#     word_id = getmeta(y).word_id - 1
#     return updatemeta(y, (word_id = word_id,))
# end

struct MixedTokenization{T <:Tuple} <: AbstractTokenization
    ts::T
end
MixedTokenization(ts...) = MixedTokenization(ts)

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
tokenization(tkr::NaiveIndexedMatchTokenizer) = IndexedMatchTokenization(tkr.patterns)


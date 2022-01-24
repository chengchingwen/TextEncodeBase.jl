struct MixedTokenization{T <:Tuple} <: AbstractTokenization
    ts::T
end
MixedTokenization(ts...) = MixedTokenization(ts)

Base.getindex(t::MixedTokenization, i) = t.ts[i]

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
tokenization(tkr::NaiveIndexedMatchTokenizer) = IndexedTokenization()

splitting(tkr::NaiveIndexedMatchTokenizer, ::IndexedTokenization, s::SentenceStage) = splitting(tkr, MatchTokenization(tkr.patterns), s)

wrap(tkr::NaiveIndexedMatchTokenizer, t::IndexedTokenization, s::SentenceStage, (i, x)) = updatemeta(wrap(tkr, MatchTokenization(tkr.patterns), s, x), (offsets = i,))

tokenize(tkr::NaiveIndexedMatchTokenizer, t::IndexedTokenization, ::Nothing, w::WordStage) = tokenize(tkr, MatchTokenization(tkr.patterns), nothing, w)

splitting(tkr::NaiveIndexedMatchTokenizer, ::MatchTokenization, w::WordStage, x) = splitting(tkr, IndexedTokenization(), w, x)

wrap(tkr::NaiveIndexedMatchTokenizer, t::MatchTokenization, w::WordStage, (i, x)) = updatemeta(wrap(t, w, x), (offsets = i,))

function wrap(tkr::NaiveIndexedMatchTokenizer, t::MatchTokenization, w::TokenStage)
    y = wrap(tkr, IndexedTokenization(), w)
    word_id = getmeta(y).word_id - 1
    return updatemeta(y, (word_id = word_id,))
end

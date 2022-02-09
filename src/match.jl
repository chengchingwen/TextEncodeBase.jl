struct MatchTokenization{T<:AbstractTokenization} <: WrappedTokenization{T}
    base::T
    patterns::Vector{Regex}
end
MatchTokenization(patterns) = MatchTokenization(DefaultTokenization(), patterns)

@inline splitting(p::ParentStages, t::MatchTokenization, x::SubSentence) = splitting(p, t.base, Sentence(getvalue(x), getmeta(x)))
@inline splitting(p::ParentStages, t::MatchTokenization, s::SentenceStage) = matchsplits(t.patterns, getvalue(s))

@inline wrap(p::ParentStages, t::MatchTokenization, s::SentenceStage, (istoken, x)) = istoken ? Token(x, getmeta(s)) : SubSentence(x, getmeta(s))
@inline wrap(p::ParentStages, t::MatchTokenization, s::TokenStages, x) = wrap(t.base, s, x)
@inline wrap(p::ParentStages, t::MatchTokenization, x::TokenStages) = wrap(t.base, x)

# calling directly on word should check if any match exists
splittability(::Nothing, ::MatchTokenization, ::WordStage) = Splittable()
@inline splitting(::Nothing, t::MatchTokenization, w::WordStage) = matchsplits(t.patterns, getvalue(w))
@inline wrap(::Nothing, t::MatchTokenization, w::WordStage, (istoken, x)) = istoken ? Token(x, getmeta(w)) : Word(x, getmeta(w))

function wrap(p::ParentStages, t::IndexedTokenization{MatchTokenization}, w::TokenStage)
    y = wrap(p, t.base, w)
    word_id = getmeta(y).word_id - 1
    return updatemeta(y, (word_id = word_id,))
end

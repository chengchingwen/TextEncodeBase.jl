struct MatchTokenization{T<:AbstractTokenization} <: WrappedTokenization{T}
    base::T
    patterns::Vector{Regex}
end
MatchTokenization(patterns) = MatchTokenization(DefaultTokenization(), patterns)
MatchTokenization(base, patterns) = MatchTokenization(base, map(as_match, patterns))

@inline splitting(p::ParentStages, t::MatchTokenization, x::SubSentence) = splitting(p, t.base, Sentence(getvalue(x), getmeta(x)))
@inline splitting(p::ParentStages, t::MatchTokenization, s::SentenceStage) = MatchSplits(t.patterns, getvalue(s))

@inline function wrap(p::ParentStages, t::MatchTokenization, s::SentenceStage, (istoken, x))
    meta = updatemeta(getmeta(s), (ismatch = istoken,))
    return istoken ? Token(x, meta) : SubSentence(x, meta)
end
@inline wrap(p::ParentStages, t::MatchTokenization, s::TokenStages, x) = wrap(p, t.base, s, x)
@inline wrap(p::ParentStages, t::MatchTokenization, x::TokenStages) = wrap(p, t.base, x)

# calling directly on word should check if any match exists
splittability(::Nothing, ::MatchTokenization, ::WordStage) = Splittable()
@inline splitting(::Nothing, t::MatchTokenization, w::WordStage) = MatchSplits(t.patterns, getvalue(w))
@inline function wrap(::Nothing, t::MatchTokenization, w::WordStage, (istoken, x))
    meta = updatemeta(getmeta(w), (ismatch = istoken,))
    return istoken ? Token(x, meta) : Word(x, meta)
end

# show
function Base.show(io::IO, t::MatchTokenization)
    print(io, "MatchTokenization(")
    show(io, t.base)
    print(io, ", ", length(t.patterns), " patterns)")
end

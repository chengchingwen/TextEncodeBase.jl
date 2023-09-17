struct MatchTokenization{T<:AbstractTokenization, P <: AbstractPattern} <: WrappedTokenization{T}
    base::T
    patterns::Vector{P}
    MatchTokenization(base::T, patterns::Vector{P}) where {T <: AbstractTokenization, P <: AbstractPattern} = MatchTokenization{T}(base, patterns)
    function MatchTokenization{T}(base::T, patterns::Vector{P}) where {T <: AbstractTokenization, P <: AbstractPattern}
        return new{T, P}(base, patterns)
    end
end
MatchTokenization(patterns) = MatchTokenization(DefaultTokenization(), patterns)
MatchTokenization(base, patterns) = MatchTokenization(base, map(as_match, patterns))

Base.:(==)(a::MatchTokenization, b::MatchTokenization) = a.base == b.base && a.patterns == b.patterns

@inline splitting(p::ParentStages, t::MatchTokenization, x::SubSentence) = splitting(p, t.base, Sentence(getvalue(x), getmeta(x)))
@inline splitting(p::ParentStages, t::MatchTokenization, s::SentenceStage) = matchsplits(t.patterns, getvalue(s))

@inline function wrap(p::ParentStages, t::MatchTokenization, s::SentenceStage, (istoken, x))
    meta = updatemeta(getmeta(s), (ismatch = istoken,))
    return istoken ? Token(x, meta) : SubSentence(x, meta)
end
@inline wrap(p::ParentStages, t::MatchTokenization, s::TokenStages, x) = wrap(p, t.base, s, x)
@inline wrap(p::ParentStages, t::MatchTokenization, x::TokenStages) = wrap(p, t.base, x)

# calling directly on word should check if any match exists
splittability(::Nothing, ::MatchTokenization, ::WordStage) = Splittable()
@inline splitting(::Nothing, t::MatchTokenization, w::WordStage) = matchsplits(t.patterns, getvalue(w))
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

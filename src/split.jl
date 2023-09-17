struct EachSplitTokenization{S} <: BaseTokenization
    splitter::S
end

@static if VERSION < v"1.8"
    splitting(t::EachSplitTokenization, s::SentenceStage) = split(getvalue(s), t.splitter; keepempty = false)
else
    splitting(t::EachSplitTokenization, s::SentenceStage) = eachsplit(getvalue(s), t.splitter; keepempty = false)
end

struct EachMatchTokenization{P<:AbstractPattern} <: BaseTokenization
    pattern::P
    EachMatchTokenization(r::Regex) = new{Regex}(Base.compile(r))
    EachMatchTokenization(r::AbstractPattern) = new{typeof(r)}(r)
end
EachMatchTokenization(r) = EachMatchTokenization(as_match(r))

splitting(t::EachMatchTokenization, s::SentenceStage) = Iterators.map(x->String(x.match), eachmatch(t.pattern, getvalue(s)))

struct MatchSplitsTokenization{P <: Union{AbstractPattern, Vector{<:AbstractPattern}}} <: BaseTokenization
    pattern::P
    MatchSplitsTokenization(r::Regex) = new{Regex}(Base.compile(r))
    MatchSplitsTokenization(r::Union{AbstractPattern, Vector{<:AbstractPattern}}) = new{typeof(r)}(r)
end
MatchSplitsTokenization(r::AbstractString) = MatchSplitsTokenization(as_match(r))
MatchSplitsTokenization(r::AbstractVector) = MatchSplitsTokenization(map(as_match, r))

splitting(t::MatchSplitsTokenization, s::SentenceStage) = Iterators.map(last, matchsplits(t.pattern, getvalue(s)))

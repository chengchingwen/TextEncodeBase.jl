struct MatchTokenization <: AbstractTokenization
    patterns::Vector{Regex}
end

splitting(t::MatchTokenization, s::SentenceStage) = collect(Tuple{Bool, SubString}, matchsplits(t.patterns, s.x))

@inline tokenize(t::MatchTokenization, s::SentenceStage, (istoken, x)) = istoken ? Token(x, s.meta) : SubSentence(x, s.meta)


struct IndexedMatchTokenization <: AbstractTokenization
    patterns::Vector{Regex}
end

@inline splitting(t::IndexedMatchTokenization, s::SentenceStage) = splitting(MatchTokenization(t.patterns), s)
@inline splitting(::IndexedMatchTokenization, s::TokenStages, x) = splitting(IndexedTokenization(), s, x)

function splitting(::IndexedMatchTokenization, s::SubSentenceStage, x)
    lastid = length(x)
    !isnothing(s.meta.rsibling) && (s.meta.rsibling[] = lastid + s.meta.offset[])
    return enumerate(x)
end

function splitting(::IndexedMatchTokenization, s::SentenceStage, x)
    tokenoffset = map(Base.RefValue, 0:length(x)-1)
    RV = Base.RefValue{Int}
    v = Tuple{RV, Tuple{Bool, SubString}, Union{RV, Nothing}}[]
    for ((i, sp), offset) in zip(enumerate(x), tokenoffset)
        push!(v, (offset, sp, i == lastindex(x) ? nothing : tokenoffset[i+1]))
    end
    return v
end

function tokenize(::IndexedMatchTokenization, s::SentenceStage, (offset, (istoken, x), rsibling))
    meta = merge(s.meta, (offset = offset, rsibling = rsibling))
    return istoken ? Token(x, meta) : SubSentence(x, meta)
end

@inline tokenize(::IndexedMatchTokenization, d::DocumentStage, x) = tokenize(IndexedTokenization(), d, x)

function tokenize(::IndexedMatchTokenization, s::SubSentenceStage, (i, x))
    offset = s.meta.offset[]
    meta = Base.structdiff(s.meta, NamedTuple{(:offset, :rsibling)})
    return Token(x, merge(meta, (token_id = i+offset,)))
end

function tokenize(::IndexedMatchTokenization, x::TokenStage)
    if haskey(x.meta, :offset) && haskey(x.meta, :rsibling)
        cid = x.meta.offset[]+1
        x.meta.rsibling[] = cid
        meta = Base.structdiff(x.meta, NamedTuple{(:offset, :rsibling)})
        return [updatemeta(x, merge(meta, (token_id = cid,)))]
    end
    return [x]
end

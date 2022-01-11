# match utils

struct MatchSplitIterator
    t::Regex
    s::Union{String, SubString}
end
Base.eltype(::Type{MatchSplitIterator}) = Tuple{Bool, SubString}
Base.IteratorSize(::Type{MatchSplitIterator}) = Base.SizeUnknown()

function Base.iterate(itr::MatchSplitIterator, (r, i, e) = (nothing, firstindex(itr.s), lastindex(itr.s)))
    i > e && return nothing
    t, s = itr.t, itr.s
    if !isnothing(r)
        ri, re = first(r), last(r)
        j = isempty(r) ? first(r) : last(r)
        v = (true, SubString(s, ri, re))
        return v, j > e ? (nothing, i, -1) : (nothing, @inbounds(nextind(s, j)), e)
    end
        
    r = findnext(itr.t, itr.s, i)
    if isnothing(r)
        return (false, SubString(s, i, e)), (nothing, i, -1)
    end

    ri, re = first(r), last(r)
    if i != ri
        return (false, SubString(s, i, @inbounds(prevind(s, ri)))), (r, i, e)
    else
        j = isempty(r) ? first(r) : last(r)
        v = (true, SubString(s, ri, re))
        return v, j > e ? (nothing, i, -1) : (nothing, @inbounds(nextind(s, j)), e)
    end
    nothing
end

matchsplit(t, s) = matchsplit!(Tuple{Bool, SubString}[], t, s)
function matchsplit!(found, t, s)
    i, e = firstindex(s), lastindex(s)

    while true
        r = findnext(t, s, i)
        if isnothing(r)
            push!(found, (false, SubString(s, i, e)))
            break
        end

        ri, re = first(r), last(r)
        i != ri && push!(found, (false, @inbounds SubString(s, i, prevind(s, ri))))
        push!(found, (true, SubString(s, ri, re)))

        j = isempty(r) ? first(r) : last(r)
        j > e && break
        @inbounds i = nextind(s, j)
        i > e && break
    end
    return found
end

function matchsplits(patterns, x)
    m, ms = first(patterns), @view patterns[2:end]
    sp = MatchSplitIterator(m, x)

    for m in ms
        iters = Iterators.map(sp) do (istoken, s)
            istoken ? ((istoken, s) for _ = 1:1) : MatchSplitIterator(m, s)
        end
        sp = Iterators.Flatten(iters)
    end
    return sp
end

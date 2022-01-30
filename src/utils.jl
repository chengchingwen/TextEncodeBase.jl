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

# misc

struct FixRest{F, A<:Tuple}
    f::F
    arg::A
end
FixRest(f, arg...) = FixRest(f, arg)

(f::FixRest)(arg...) = f.f(arg..., f.arg...)

nestedcall(f) = Base.Fix1(nestedcall, f)
nestedcall(f, x::AbstractArray) = map(nestedcall(f), x)
nestedcall(f, x) = f(x)

# encode utils

"""
    with_head_tail(x, head, tail)

Return `[head; x; tail]`. Ignored if `head` or `tail` is `nothing`.

# Example

```julia
julia> TextEncodeBase.with_head_tail(1:5, -1, -2)
7-element Vector{Int64}:
 -1
  1
  2
  3
  4
  5
 -2

julia> TextEncodeBase.with_head_tail([1:5, 2:3], -1, -2)
2-element Vector{Vector{Int64}}:
 [-1, 1, 2, 3, 4, 5, -2]
 [-1, 2, 3, -2]

```
"""
function with_head_tail(x, head, tail)
    n, T = length(x), eltype(x)
    !isnothing(head) && ((n, T) = (n+1, promote_type(T, typeof(head))))
    !isnothing(tail) && ((n, T) = (n+1, promote_type(T, typeof(tail))))
    vec = Vector{T}(undef, n); empty!(vec)
    !isnothing(head) && push!(vec, head)
    append!(vec, x)
    !isnothing(tail) && push!(vec, tail)
    return vec
end
with_head_tail(x::AbstractArray{<:AbstractVector}, head, tail) = map(with_head_tail(head, tail), x)
with_head_tail(head, tail) = FixRest(with_head_tail, head, tail)
with_head_tail(x; head=nothing, tail=nothing) = with_head_tail(x, head, tail)
with_head_tail(; head=nothing, tail=nothing) = with_head_tail(head, tail)

"""
    trunc_and_pad(x, n, pad)

truncate `x` to length `n`, otherwise add `pad` at the end of x until length equal `n`.
 `x` can be either nested or single array (but the element type should not be subtype of abstract array).
 if `n` is `nothing`, the largest length of the nested array will be used.

# Example

```julia
julia> TextEncodeBase.trunc_and_pad(1:5, 7, -1)
7-element Vector{Int64}:
  1
  2
  3
  4
  5
 -1
 -1

julia> TextEncodeBase.trunc_and_pad([1:5, 2:7], 7, -1)
2-element Vector{Vector{Int64}}:
 [1, 2, 3, 4, 5, -1, -1]
 [2, 3, 4, 5, 6, 7, -1]

julia> TextEncodeBase.trunc_and_pad([1:5, [2:7, [1:2]]], nothing, -1)
2-element Vector{Vector}:
 [1, 2, 3, 4, 5, -1]
 Vector[[2, 3, 4, 5, 6, 7], [[1, 2, -1, -1, -1, -1]]]

```
"""
trunc_and_pad(x, n::Integer, pad) = trunc_and_pad!(similar(x, n), x, n, pad)
trunc_and_pad(x::AbstractArray{<:AbstractVector}, n::Integer, pad) = map(trunc_and_pad(n, pad), x)
trunc_and_pad(x, ::Nothing, pad) = trunc_and_pad(x, nestedmaxlength(x), pad)
trunc_and_pad(n, pad) = FixRest(trunc_and_pad, n, pad)
function trunc_and_pad!(vec, x, n, pad)
    if length(x) <= n
        copyto!(vec, x)
        vec[length(x)+1:n] .= pad
    else
        copyto!(vec, 1, x, 1, n)
    end
    return vec
end

nestedmaxlength(x::AbstractArray{<:AbstractArray}) = mapfoldl(nestedmaxlength, max, x)
nestedmaxlength(x::AbstractArray) = length(x)

_checkeq(x, y, msg) = x == y ? x : error(msg, ": $x != $y")
_checkeq(msg) = FixRest(_checkeq, msg)

function nestedsize(x::AbstractArray{<:AbstractArray})
    dim = size(x)
    return (mapfoldl(nestedsize, _checkeq("nested size not the same"), x)..., dim...)
end
nestedsize(x::AbstractArray) = size(x)

nestedtype(x::AbstractArray{<:AbstractArray}) = mapreduce(nestedtype, promote_type, x)
nestedtype(x::AbstractArray) = mapreduce(typeof, promote_type, x)

"""
    nested2batch(x)

convert nested array into single array

# Example

```julia
julia> TextEncodeBase.nested2batch([[[1 2],[3 4]]])
1×2×2×1 Array{Int64, 4}:
[:, :, 1, 1] =
 1  2

[:, :, 2, 1] =
 3  4

```
"""
function nested2batch(x)
    arr = Array{nestedtype(x)}(undef, nestedsize(x))
    _nested2batch!(arr, 1, x)
    return arr
end

_reduce_nested(dst_offset, xi) = dst_offset[1], _nested2batch!(dst_offset..., xi)[2]
_nested2batch!(arr, offset, x::AbstractArray{<:AbstractArray}) = foldl(_reduce_nested, x; init=(arr, offset))
_nested2batch!(arr, offset, x::AbstractArray) = (copyto!(arr, offset, x, 1, length(x)); (arr, offset+length(x)))

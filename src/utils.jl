using FuncPipelines: FixRest

using Base.PCRE

# match utils

literal_match_regex(s::Union{AbstractString, AbstractChar}, flags...) = Regex(Base.wrap_string(s, UInt32(0)), flags...)

as_match(r::Regex) = r
as_match(s::Union{AbstractString, AbstractChar}) = literal_match_regex(s)

mutable struct MatchSplitIterator
    regex::Regex
    opts::UInt32
    str::SubString{String}
    lastidx::Int
    conti::Bool
    data::Ptr{Nothing}
    ismatch::Bool
    i::Int
    ri::Int
    function MatchSplitIterator(regex::Regex, str::SubString)
        Base.compile(regex)
        data = PCRE.create_match_data(regex.regex)
        itr = new(regex, regex.match_options, str, lastindex(str), true, data, true, firstindex(str), 0)
        finalizer(itr) do itr
            itr.data == C_NULL || PCRE.free_match_data(itr.data)
        end
        return itr
    end
end
MatchSplitIterator(regex::Regex, str::String) = MatchSplitIterator(regex, SubString(str))
MatchSplitIterator(regex, str) = MatchSplitIterator(literal_match_regex(regex), str)

Base.eltype(::Type{MatchSplitIterator}) = Tuple{Bool, SubString{String}}
Base.IteratorSize(::Type{MatchSplitIterator}) = Base.SizeUnknown()

Base.iterate(itr::MatchSplitIterator) = Base.iterate(itr, nothing)
function Base.iterate(itr::MatchSplitIterator, _)
    itr.conti || return nothing
    e = itr.lastidx
    v, nstate = _matchsplit(itr.regex, itr.str, itr.i, e, itr.opts, itr.data, itr.ismatch, itr.ri)
    i = nstate[1]
    itr.i = i
    if i > e
        itr.conti = false
        return v, nothing
    else
        itr.ismatch = v[1]
        itr.ri = nstate[2]
        return v, nothing
    end
end

matchsplit(t, s) = matchsplit!(Tuple{Bool, SubString{String}}[], t, s)
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

@inline function _matchsplit(reg::Regex, s, i, e, opts, data, ismatch, ri)
    if ri == 0
        matched = PCRE.exec(reg.regex, s, i - 1, opts, data)
        if matched
            p = PCRE.ovec_ptr(data)
            ri = Int(unsafe_load(p,1))+1
        else
            return (false, @inbounds(SubString(s, i, e))), (e+1, 0)
        end
    end

    if ismatch && i != ri
        return (false, @inbounds(SubString(s, i, prevind(s, ri)))), (i, ri)
    end

    p = PCRE.ovec_ptr(data)
    re = @inbounds prevind(s, Int(unsafe_load(p,2))+1)
    i = @inbounds nextind(s, re)
    return (true, @inbounds(SubString(s, ri, re))), (i, 0)
end

function matchsplit!(found, reg::Regex, s)
    i, e = firstindex(s), lastindex(s)
    Base.compile(reg)
    data = PCRE.create_match_data(reg.regex)
    opts = reg.match_options

    ismatch = true
    ri = 0
    while true
        v, state = _matchsplit(reg, s, i, e, opts, data, ismatch, ri)
        push!(found, v)
        i = state[1]
        i > e && break
        ismatch = v[1]
        ri = state[2]
        # matched = PCRE.exec(reg.regex, s, i - 1, opts, data)
        # if matched
        #     p = PCRE.ovec_ptr(data)
        #     ri = Int(unsafe_load(p,1))+1
        #     re = @inbounds prevind(s, Int(unsafe_load(p,2))+1)
        # else
        #     push!(found, (false, SubString(s, i, e)))
        #     break
        # end
        # i != ri && push!(found, (false, @inbounds SubString(s, i, prevind(s, ri))))
        # push!(found, (true, SubString(s, ri, re)))
        # @inbounds i = nextind(s, re)
        # i > e && break
    end
    PCRE.free_match_data(data)
    return found
end

matchsplits(t, s) = matchsplits!(Tuple{Bool, SubString{String}}[], t, s)
function matchsplits!(found, regs, s)
    if isempty(regs)
        push!(found, (false, s))
        return found
    end
    reg = as_match(first(regs))
    rest = @view regs[2:end]
    for v in MatchSplitIterator(reg, s)
        ismatch = v[1]
        if ismatch
            push!(found, v)
        else
            matchsplits!(found, rest, v[2])
        end
    end
    return found
end

function _MatchSplits(patterns, x)
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

struct MatchSplits
    regexes::Vector{Regex}
    n::Int
    str::SubString{String}
    function MatchSplits(regexes::Vector{Regex}, str::SubString{String})
        n = length(regexes)
        @assert n != 0
        return new(regexes, n, str)
    end
end
MatchSplits(regexes::Vector{Regex}, str::String) = MatchSplits(regexes, SubString(str))
MatchSplits(regexes, str) = MatchSplits(map(as_match, regexes), str)

Base.eltype(::Type{MatchSplits}) = Tuple{Bool, SubString{String}}
Base.IteratorSize(::Type{MatchSplits}) = Base.SizeUnknown()

function Base.iterate(itr::MatchSplits)
    itr1 = MatchSplitIterator(itr.regexes[1], itr.str)
    n = itr.n
    state = [itr1]
    return Base.iterate(itr, state)
end

function Base.iterate(itr::MatchSplits, state)
    level = length(state)
    iszero(level) && return nothing
    itr_i = @inbounds state[level]
    I = Base.iterate(itr_i)
    if isnothing(I)
        pop!(state)
        return Base.iterate(itr, state)
    end
    v, _ = I
    ismatch = v[1]
    if ismatch
        return v, state
    else
        if level == itr.n
            return v, state
        else
            regex_j = @inbounds itr.regexes[level+1]
            itr_j = MatchSplitIterator(regex_j, v[2])
            push!(state, itr_j)
            return Base.iterate(itr, state)
        end
    end
end

# misc

nestedcall(f) = Base.Fix1(nestedcall, f)
nestedcall(f, x::AbstractArray) = map(nestedcall(f), x)
nestedcall(f, x) = f(x)

function _nestedcall_f!(f, ys, xs)
    @inbounds for i in eachindex(xs, ys)
        ys[i] = nestedcall(f, xs[i])
    end
    return ys
end

function _nestedcall_f_fallback!(f, ys, xs)
    S = Union{}
    @inbounds for i in eachindex(xs, ys)
        ys[i] = nestedcall(f, xs[i])
        S = promote_type(S, typeof(ys[i]))
    end
    return S, ys
end

function nestedcall(f, xs::Array)
    R = Core.Compiler.return_type(nestedcall, Tuple{typeof(f), eltype(xs)})
    if Base.isconcretetype(R)
        return _nestedcall_f!(f, similar(xs, R), xs)
    else
        S, ys = _nestedcall_f_fallback!(f, similar(xs, R), xs)
        if S != R
            zs = similar(xs, S)
            copyto!(zs, ys)
            return zs
        end
        return ys
    end
end

# encode utils

allany(f, x) = mapfoldl(f, _allany, x; init=(true, false))
_allany(a, b) = a[1] & b, a[2] | !b

"""
    with_head_tail(x, head, tail)

Return `[head; x; tail]`. Ignored if `head` or `tail` is `nothing`. `x` can be nested arrays.

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
with_head_tail(x::AbstractArray, head, tail) = _with_head_tail(x, head, tail)
with_head_tail(x::AbstractArray{<:AbstractVector}, head, tail) = map(with_head_tail(head, tail), x)
function with_head_tail(x::AbstractArray{>:AbstractArray}, head, tail)
    aoa, aov = allany(Base.Fix2(isa, AbstractArray), x)
    if aoa
        map(with_head_tail(head, tail), x)
    elseif aov
        T = mapreduce(typeof, promote_type, x)
        _with_head_tail(T, x, head, tail)
    else
        error("Input array is mixing array and non-array elements")
    end
end

with_head_tail(head, tail) = FixRest(with_head_tail, head, tail)
with_head_tail(x; head=nothing, tail=nothing) = with_head_tail(x, head, tail)
with_head_tail(; head=nothing, tail=nothing) = with_head_tail(head, tail)

@inline function _with_head_tail(::Type{T}, x, head, tail) where T
    S = T
    n = length(x)
    !isnothing(head) && ((n, S) = (n+1, promote_type(S, typeof(head))))
    !isnothing(tail) && ((n, S) = (n+1, promote_type(S, typeof(tail))))
    vec = Vector{S}(undef, n); empty!(vec)
    !isnothing(head) && push!(vec, head)
    append!(vec, x)
    !isnothing(tail) && push!(vec, tail)
    return vec
end
_with_head_tail(x, head, tail) = _with_head_tail(eltype(x), x, head, tail)

"""
    trunc_or_pad(x, n, pad)

Truncate `x` to length `n`, or add `pad` at the end of x until length equal `n`.
 `x` can be either nested or single array. if `n` is `nothing`, the largest length of
 the inner-most array will be used.

    trunc_or_pad(n, pad)

Create a function that will return new array with truncated or padded value of the input.

see also: [`trunc_and_pad`](@ref)

# Example

```julia
julia> TextEncodeBase.trunc_or_pad(1:5, 7, -1)
7-element Vector{Int64}:
  1
  2
  3
  4
  5
 -1
 -1

julia> TextEncodeBase.trunc_or_pad([1:5, 2:7], 10, -1)
2-element Vector{Vector{Int64}}:
 [1, 2, 3, 4, 5, -1, -1, -1, -1, -1]
 [2, 3, 4, 5, 6, 7, -1, -1, -1, -1]

julia> TextEncodeBase.trunc_or_pad([1:5, [2:7, [1:2]]], nothing, -1)
2-element Vector{Vector}:
 [1, 2, 3, 4, 5, -1]
 Vector[[2, 3, 4, 5, 6, 7], [[1, 2, -1, -1, -1, -1]]]

```
"""
trunc_or_pad(x::AbstractArray, n::Integer, pad) = trunc_or_pad!(similar(x, n), x, n, pad)
trunc_or_pad(x::AbstractArray{<:AbstractArray}, n::Integer, pad) = map(trunc_or_pad(n, pad), x)
function trunc_or_pad(x::AbstractArray{>:AbstractArray}, n::Integer, pad)
    aoa, aov = allany(Base.Fix2(isa, AbstractArray), x)
    if aoa
        map(trunc_or_pad(n, pad), x)
    elseif aov
        trunc_or_pad!(similar(x, n), x, n, pad)
    else
        error("Input array is mixing array and non-array elements")
    end
end
trunc_or_pad(x, ::Nothing, pad) = trunc_or_pad(x, nestedmaxlength(x), pad)

trunc_or_pad(n, pad) = FixRest(trunc_or_pad, n, pad)
trunc_or_pad(x; n=nothing, pad) = trunc_or_pad(x, n, pad)
trunc_or_pad(; n=nothing, pad) = trunc_or_pad(n, pad)

function trunc_or_pad!(vec, x, n, pad)
    if length(x) <= n
        copyto!(vec, x)
        vec[length(x)+1:n] .= pad
    else
        copyto!(vec, 1, x, 1, n)
    end
    return vec
end

"""
    trunc_and_pad(x, maxn, pad)

Truncate `x` if length exceed `maxn`, and add `pad` at the end of x until all length are the same.
 `x` can be either nested or single array. If `maxn` is `nothing`, the largest length of
 the inner-most array will be used, then the behavior equals to `trunc_or_pad` with `nothing`.

    trunc_and_pad(maxn, pad)

Create a function that truncate input to be length <= `maxn`, and add `pad` until all input has equal length.

see also: [`trunc_or_pad`](@ref)

# Example

```julia
julia> TextEncodeBase.trunc_and_pad(1:5, 7, -1)
5-element Vector{Int64}:
 1
 2
 3
 4
 5

julia> TextEncodeBase.trunc_and_pad([1:5, 2:7], 10, -1)
2-element Vector{Vector{Int64}}:
 [1, 2, 3, 4, 5, -1]
 [2, 3, 4, 5, 6, 7]

julia> TextEncodeBase.trunc_and_pad([1:5, [2:7, [1:2]]], nothing, -1)
2-element Vector{Vector}:
 [1, 2, 3, 4, 5, -1]
 Vector[[2, 3, 4, 5, 6, 7], [[1, 2, -1, -1, -1, -1]]]

```
"""
trunc_and_pad(x, maxn, pad) = (n = nestedmaxlength(x); _trunc_and_pad(x, n, isnothing(maxn) ? n : maxn, pad))

trunc_and_pad(maxn, pad) = FixRest(trunc_and_pad, maxn, pad)
trunc_and_pad(x; maxn=nothing, pad) = trunc_and_pad(x, maxn, pad)
trunc_and_pad(; maxn=nothing, pad) = trunc_and_pad(maxn, pad)

_trunc_and_pad(x, n, maxn, pad) = trunc_or_pad(x, min(n, maxn), pad)

nestedmaxlength(x::AbstractArray{<:AbstractArray}) = mapfoldl(nestedmaxlength, max, x)
nestedmaxlength(x::AbstractArray) = length(x)
function nestedmaxlength(x::AbstractArray{>:AbstractArray})
    aoa, aov = allany(Base.Fix2(isa, AbstractArray), x)
    if aoa
        mapfoldl(nestedmaxlength, max, x)
    elseif aov
        length(x)
    else
        error("Input array is mixing array and non-array elements")
    end
end

_checkeqsize(x, y) = x == y ? x : throw(DimensionMismatch("nested size not the same: $x != $y"))

nestedsize(x::AbstractArray{<:AbstractArray}) = (mapfoldl(nestedsize, _checkeqsize, x)..., size(x)...)
nestedsize(x::AbstractArray) = size(x)
function nestedsize(x::AbstractArray{>:AbstractArray})
    aoa, aov = allany(Base.Fix2(isa, AbstractArray), x)
    if aoa
        (mapfoldl(nestedsize, _checkeqsize, x)..., size(x)...)
    elseif aov
        size(x)
    else
        error("Input array is mixing array and non-array elements")
    end
end

nestedtype(x::AbstractArray{<:AbstractArray}) = mapreduce(nestedtype, promote_type, x)
nestedtype(x::AbstractArray) = mapreduce(typeof, promote_type, x)
function nestedtype(x::AbstractArray{>:AbstractArray})
    aoa, aov = allany(Base.Fix2(isa, AbstractArray), x)
    if aoa
        mapreduce(nestedtype, promote_type, x)
    elseif aov
        mapreduce(typeof, promote_type, x)
    else
        error("Input array is mixing array and non-array elements")
    end
end


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
function _nested2batch!(arr, offset, x::AbstractArray{>:AbstractArray})
    aoa, aov = allany(Base.Fix2(isa, AbstractArray), x)
    if aoa
        foldl(_reduce_nested, x; init=(arr, offset))
    elseif aov
        copyto!(arr, offset, x, 1, length(x))
        (arr, offset+length(x))
    else
        error("Input array is mixing array and non-array elements")
    end
end

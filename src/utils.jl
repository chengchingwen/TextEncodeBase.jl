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

    trunc_or_pad(x, n, pad, trunc_end = :tail, pad_end = :tail)

`trunc_end` and `pad_end` specified whether the truncation and padding happened at the begining of the
 sentences or the end of the sentence. The value is either `:tail` (means the end) or `:head` (means the
 begining).

    trunc_or_pad(n, pad, trunc_end = :tail, pad_end = :tail)

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
trunc_or_pad(x::AbstractArray, n::Integer, pad, trunc_end::Symbol = :tail, pad_end::Symbol = :tail) =
    trunc_or_pad!(similar(x, n), x, n, pad, trunc_end, pad_end)
trunc_or_pad(x::AbstractArray{<:AbstractArray}, n::Integer, pad, trunc_end::Symbol = :tail, pad_end::Symbol = :tail) =
    map(trunc_or_pad(n, pad, trunc_end, pad_end), x)
function trunc_or_pad(
    x::AbstractArray{>:AbstractArray}, n::Integer, pad,
    trunc_end::Symbol = :tail, pad_end::Symbol = :tail,
)
    aoa, aov = allany(Base.Fix2(isa, AbstractArray), x)
    if aoa
        map(trunc_or_pad(n, pad, trunc_end, pad_end), x)
    elseif aov
        trunc_or_pad!(similar(x, n), x, n, pad, trunc_end, pad_end)
    else
        error("Input array is mixing array and non-array elements")
    end
end
trunc_or_pad(x, ::Nothing, pad, trunc_end::Symbol = :tail, pad_end::Symbol = :tail) =
    trunc_or_pad(x, nestedmaxlength(x), pad, trunc_end, pad_end)
trunc_or_pad(n, pad, trunc_end::Symbol = :tail, pad_end::Symbol = :tail) =
    FixRest(trunc_or_pad, n, pad, trunc_end, pad_end)
trunc_or_pad(x; n = nothing, pad, trunc_end::Symbol = :tail, pad_end::Symbol = :tail) =
    trunc_or_pad(x, n, pad, trunc_end, pad_end)
trunc_or_pad(; n = nothing, pad, trunc_end::Symbol = :tail, pad_end::Symbol = :tail) =
    trunc_or_pad(n, pad, trunc_end, pad_end)

function trunc_or_pad!(vec, x, n, pad, trunc_end, pad_end)
    len = length(x)
    if len <= n # pad
        if pad_end == :tail
            copyto!(vec, x)
            vec[len+1:n] .= pad
        elseif pad_end == :head
            pad_prefix_size = n - len
            copyto!(vec, pad_prefix_size + 1, x, 1, len)
            vec[1:pad_prefix_size] .= pad
        else
            error("`pad_end` is not :head or :tail but: $pad_end")
        end
    else # trunc
        if trunc_end == :tail
            copyto!(vec, 1, x, 1, n)
        elseif trunc_end == :head
            copyto!(vec, 1, x, len - n + 1, n)
        else
            error("`trunc_end` is not :head or :tail but: $trunc_end")
        end
    end
    return vec
end

"""
    trunc_and_pad(x, maxn, pad)

Truncate `x` if length exceed `maxn`, and add `pad` at the end of x until all length are the same.
 `x` can be either nested or single array. If `maxn` is `nothing`, the largest length of
 the inner-most array will be used, then the behavior equals to `trunc_or_pad` with `nothing`.

    trunc_and_pad(x, maxn, pad, trunc_end = :tail, pad_end = :tail)

`trunc_end` and `pad_end` specified whether the truncation and padding happened at the begining of the
 sentences or the end of the sentence. The value is either `:tail` (means the end) or `:head` (means the
 begining).

    trunc_and_pad(maxn, pad, trunc_end = :tail, pad_end = :tail)

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
trunc_and_pad(x, maxn, pad, trunc_end::Symbol = :tail, pad_end::Symbol = :tail) =
    (n = nestedmaxlength(x); _trunc_and_pad(x, n, isnothing(maxn) ? n : maxn, pad, trunc_end, pad_end))

trunc_and_pad(maxn, pad, trunc_end::Symbol = :tail, pad_end::Symbol = :tail) =
    FixRest(trunc_and_pad, maxn, pad, trunc_end, pad_end)
trunc_and_pad(x; maxn=nothing, pad, trunc_end::Symbol = :tail, pad_end::Symbol = :tail) =
    trunc_and_pad(x, maxn, pad, trunc_end, pad_end)
trunc_and_pad(; maxn=nothing, pad, trunc_end::Symbol = :tail, pad_end::Symbol = :tail) =
    trunc_and_pad(maxn, pad, trunc_end, pad_end)

@inline _trunc_and_pad(x, n, maxn, pad, trunc_end, pad_end) = trunc_or_pad(x, min(n, maxn), pad, trunc_end, pad_end)

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

# Sequence template

abstract type TemplateTerm{T} end


"""
    InputTerm{T}(type_id = 1)

A `TemplateTerm` that take out a sequence from the input.
"""
struct InputTerm{T} <: TemplateTerm{T}
    type_id::Int
    InputTerm{T}(type_id = 1) where T = new{T}(type_id)
end

"""
    IndexInputTerm{T}(idx::Int, type_id = 1)

A `TemplateTerm` that take the `idx`-th sequence of the input. If the `IndexInputTerm` is also the `idx`-th
 input related term in a [`SequenceTemplate`](@ref), it behave the same as [`InputTerm`](@ref).
"""
struct IndexInputTerm{T} <: TemplateTerm{T}
    idx::Int
    type_id::Int
    IndexInputTerm{T}(idx, type_id = 1) where T = new{T}(idx, type_id)
end

"""
    ConstTerm(value::T, type_id = 1)

A `TemplateTerm` that simply put `value` to the output sequence.
"""
struct ConstTerm{T} <: TemplateTerm{T}
    value::T
    type_id::Int
end
ConstTerm(value, type_id = 1) = ConstTerm{typeof(value)}(value, type_id)

"""
    RepeatedTerm(terms::TemplateTerm...)

A special term that indicate the `terms` sequence can appear zero or multiple times. Cannot be nested.
"""
struct RepeatedTerm{T, Ts<:Tuple{Vararg{TemplateTerm{T}}}} <: TemplateTerm{T}
    terms::Ts
    function RepeatedTerm(terms::Tuple{Vararg{TemplateTerm{T}}}) where T
        @assert length(terms) >= 1 "No TemplateTerm provided."
        @assert !any(Base.Fix2(isa, RepeatedTerm), terms) "Cannot nest RepeatedTerm"
        return new{T, typeof(terms)}(terms)
    end
end
RepeatedTerm(terms::TemplateTerm...) = RepeatedTerm(terms)

"""
    SequenceTemplate(terms::TemplateTerm)(sequences...)

Constructing a function by multiple `TemplateTerm` that indicate how to combine the input `sequences`. Return
 a tuple of the result sequence and a type id (a special number associated with the template term) sequence.

# Example

```julia-repl
julia> SequenceTemplate(ConstTerm(-1), InputTerm{Int}(), ConstTerm(-2))(1:5)[1] == TextEncodeBase.with_head_tail(1:5, -1, -2)
true

julia> SequenceTemplate(ConstTerm(-1), InputTerm{Int}(), ConstTerm(-2))(1:5)
([-1, 1, 2, 3, 4, 5, -2], [1, 1, 1, 1, 1, 1, 1])

julia> bert_template = SequenceTemplate(
           ConstTerm("[CLS]", 1), InputTerm{String}(1), ConstTerm("[SEP]", 1),
           RepeatedTerm(InputTerm{String}(2), ConstTerm("[SEP]", 2))
       )
SequenceTemplate{String}([CLS]:<type=1> Input:<type=1> [SEP]:<type=1> (Input:<type=2> [SEP]:<type=2>)...)

julia> bert_template(["hello", "world"])
(["[CLS]", "hello", "world", "[SEP]"], [1, 1, 1, 1])

julia> bert_template(["hello", "world"], ["today", "is", "a", "good", "day"])
(["[CLS]", "hello", "world", "[SEP]", "today", "is", "a", "good", "day", "[SEP]"], [1, 1, 1, 1, 2, 2, 2, 2, 2, 2])

```
"""
struct SequenceTemplate{T, Ts<:Tuple{Vararg{TemplateTerm{T}}}} <: Function
    terms::Ts
    function SequenceTemplate(terms::Tuple{Vararg{TemplateTerm{T}}}) where T
        @assert length(terms) >= 1 "No TemplateTerm provided."
        @assert count(Base.Fix2(isa, RepeatedTerm), terms) <= 1 "RepeatedTerm can only appear at most once."
        return new{T, typeof(terms)}(terms)
    end
end
SequenceTemplate(terms::TemplateTerm...) = SequenceTemplate(terms)

function process_term!(term::InputTerm, output, type_ids, i, j, terms, xs)
    @assert j <= length(xs) "InputTerm indexing $j-th input but only get $(length(xs))"
    x = xs[j]
    append!(output, x)
    append!(type_ids, Iterators.repeated(term.type_id, length(x)))
    return j + 1
end

function process_term!(term::IndexInputTerm, output, type_ids, i, j, terms, xs)
    idx = term.idx
    @assert idx <= length(xs) "IndexInputTerm indexing $idx-th input but only get $(length(xs))"
    x = xs[idx]
    append!(output, x)
    append!(type_ids, Iterators.repeated(term.type_id, length(x)))
    return idx == j ? j + 1 : j
end

function process_term!(term::ConstTerm, output, type_ids, i, j, terms, xs)
    push!(output, term.value)
    push!(type_ids, term.type_id)
    return j
end

function process_term!(term::RepeatedTerm, output, type_ids, i, j, terms, xs)
    r_terms = term.terms
    n = count(Base.Fix2(isa, InputTerm), terms[i+1:end])
    J = length(xs) - n
    while j <= J
        _j = j
        for (t_i, term_i) in enumerate(r_terms)
            j = process_term!(term_i, output, type_ids, t_i, j, r_terms, xs)
        end
        _j == j && error("RepeatedTerm doesn't seem to terminate")
    end
    return j
end

apply_template(st::SequenceTemplate) = Base.Fix1(apply_template, st)
function apply_template(st::SequenceTemplate{T}, xs) where T
    terms = st.terms
    len = length(xs)
    n_input = count(Base.Fix2(isa, InputTerm), terms)
    @assert len >= n_input "SequenceTemplate require at least $n_input but only get $len"

    output = Vector{T}()
    type_ids = Vector{Int}()

    j = 1
    for (i, term) in enumerate(terms)
         j = process_term!(term, output, type_ids, i, j, terms, xs)
    end
    @assert j > len "SequenceTemplate only take $(j-1) inputs but get $len"
    return output, type_ids
end

## static single sample
(st::SequenceTemplate{T})(xs::AbstractVector{T}...) where T = apply_template(st, xs)
(st::SequenceTemplate{T})(xs::Tuple{Vararg{AbstractVector{T}}}) where T = apply_template(st, xs)
(st::SequenceTemplate{T})(xs::AbstractVector{<:AbstractVector{T}}) where T = apply_template(st, xs)

## static multiple sample
(st::SequenceTemplate{T})(xs::AbstractArray{<:AbstractVector{<:AbstractVector{T}}}) where T = map(apply_template(st), xs)

## dynamic
function (st::SequenceTemplate{T})(xs::AbstractArray) where T
    aoa, aov = allany(Base.Fix2(isa, AbstractArray), xs)
    if aoa
        if all(Base.Fix1(all, Base.Fix2(isa, T)), xs) # dynamic single sample
            # xs is an array of sequence
            return apply_template(st, xs)
        elseif all(Base.Fix1(all, Base.Fix2(isa, AbstractArray)), xs) # dynamic multiple sample
            # xs is an array of array of array
            return map(st, xs)
        else
            throw(MethodError(st, xs))
        end
    elseif aov # dynamic single sample
        # xs is a sequence
        !all(Base.Fix2(isa, T), xs) && throw(MethodError(st, xs)) # assert eltype of sequence == T
        return apply_template(st, (xs,))
    else
        throw(MethodError(st, xs))
    end
end

_show(io, t::InputTerm) = print(io, "Input:<type=$(t.type_id)>")
_show(io, t::IndexInputTerm) = print(io, "Input[$(t.idx)]:<type=$(t.type_id)>")
_show(io, t::ConstTerm) = print(io, "$(t.value):<type=$(t.type_id)>")
function _show(io, t::RepeatedTerm)
    print(io, '(')
    _show(io, first(t.terms))
    for term in Base.tail(t.terms)
        print(io, ' ')
        _show(io, term)
    end
    print(io, ")...")
end

Base.show(io::IO, ::MIME"text/plain", st::SequenceTemplate) = show(io, st)
function Base.show(io::IO, st::SequenceTemplate{T}) where T
    print(io, "SequenceTemplate{", T, "}(")
    _show(io, first(st.terms))
    for term in Base.tail(st.terms)
        print(io, ' ')
        _show(io, term)
    end
    print(io, ')')
end

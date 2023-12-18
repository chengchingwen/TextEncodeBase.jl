using DataStructures: MutableLinkedList
using FuncPipelines: FixRest
using RustRegex

using Base.PCRE

isnestedconcretetype(_) = true
@generated function isnestedconcretetype(::Type{T}) where T
    return isconcretetype(T) && all(isnestedconcretetype, T.parameters)
end

# match utils

literal_match_regex(s::Union{AbstractString, AbstractChar}, flags...) = Regex(Base.wrap_string(s, UInt32(0)), flags...)

as_match(r::AbstractPattern) = r
as_match(s::Union{AbstractString, AbstractChar}) = literal_match_regex(s)

abstract type AbstractMatchSplitIterState{P <: AbstractPattern} end

mutable struct MatchSplitIterRegexState <: AbstractMatchSplitIterState{Regex}
    i::Int
    matched::UnitRange{Int}
    data::Ptr{Nothing}
    function MatchSplitIterRegexState(regex::Regex, i = 1)
        Base.compile(regex)
        data = PCRE.create_match_data(regex.regex)
        state = new(i, 0:0, data)
        finalizer(state) do s
            s.data == C_NULL || PCRE.free_match_data(s.data)
        end
        return state
    end
end

function matchsplit_iterate!(regex::Regex, e, s, state::MatchSplitIterRegexState)
    i, matched, data = state.i, state.matched, state.data
    _regex = regex.regex
    opts = regex.match_options
    if !iszero(matched)
        str = @inbounds SubString(s, matched.start, matched.stop)
        state.matched = 0:0
        return (true, str), state
    end
    if i > e
        return nothing
    end

    if !PCRE.exec(_regex, s, i-1, opts, data)
        str = @inbounds SubString(s, i, e)
        state.i = typemax(Int)
        return (false, str), state
    end

    p = PCRE.ovec_ptr(data)
    ri = Int(unsafe_load(p, 1)) + 1
    re = prevind(s, Int(unsafe_load(p, 2)) + 1)
    matched = ri:re
    ni = nextind(s, re)
    if i != ri
        str = @inbounds SubString(s, i, prevind(s, ri))
        state.i = ni
        state.matched = matched
        return (false, str), state
    end
    str = @inbounds SubString(s, ri, re)
    state.i = ni
    state.matched = 0:0
    return (true, str), state
end

mutable struct MatchSplitIterRuRegexState <: AbstractMatchSplitIterState{RuRegex}
    i::Int
    matched::UnitRange{Int}
    itr::Ptr{Cvoid}
    function MatchSplitIterRuRegexState(regex::RuRegex, i = 1)
        obj = RustRegex.RuRE.rure_iter_new(regex)
        state = new(i, 0:0, obj)
        finalizer(state) do x
            x.itr == C_NULL || RustRegex.RuRE.rure_iter_free(x.itr)
        end
        return state
    end
end

function matchsplit_iterate!(regex::RuRegex, e, s, state::MatchSplitIterRuRegexState)
    i, matched, itr = state.i, state.matched, state.itr
    if !iszero(matched)
        str = @inbounds SubString(s, matched.start, matched.stop)
        state.matched = 0:0
        return (true, str), state
    end
    if i > e
        return nothing
    end

    m = Ref{UnitRange{UInt}}(0:0)
    len = ncodeunits(s)
    if !RustRegex.RuRE.rure_iter_next(itr, s, len, m)
        str = @inbounds SubString(s, i, e)
        state.i = typemax(Int)
        return (false, str), state
    end

    _m = m[]
    ri = thisind(s, Int(_m.start) + 1)
    re = thisind(s, Int(_m.stop))
    matched = ri:re
    ni = nextind(s, re)
    if i != ri
        str = @inbounds SubString(s, i, prevind(s, ri))
        state.i = ni
        state.matched = matched
        return (false, str), state
    end
    str = @inbounds SubString(s, ri, re)
    state.i = ni
    state.matched = 0:0
    return (true, str), state
end

struct MatchSplitIterPatternAndState{P <: AbstractPattern, S <: AbstractMatchSplitIterState{P}}
    pattern::P
    state::S
end
MatchSplitIterPatternAndState(pattern::AbstractPattern, str::SubString{String}) =
    MatchSplitIterPatternAndState(pattern, firstindex(str))

MatchSplitIterPatternAndState(regex::Regex, i::Int) = MatchSplitIterPatternAndState(regex, MatchSplitIterRegexState(regex, i))
MatchSplitIterPatternAndState(regex::RuRegex, i::Int) = MatchSplitIterPatternAndState(regex, MatchSplitIterRuRegexState(regex, i))

struct MatchSplitIterator{P<:MatchSplitIterPatternAndState}
    regex_and_state::P
    lastidx::Int
    str::SubString{String}
    function MatchSplitIterator(regex::AbstractPattern, str::SubString{String})
        regex_and_state = MatchSplitIterPatternAndState(regex, str)
        return new{typeof(regex_and_state)}(regex_and_state, lastindex(str), str)
    end
end
MatchSplitIterator(regex::AbstractPattern, str::String) = MatchSplitIterator(regex, SubString(str))
MatchSplitIterator(regex, str) = MatchSplitIterator(literal_match_regex(regex), str)

Base.eltype(::Type{<:MatchSplitIterator}) = Tuple{Bool, SubString{String}}
Base.IteratorSize(::Type{<:MatchSplitIterator}) = Base.SizeUnknown()
Base.show(io::IO, itr::MatchSplitIterator) = (print(io, "MatchSplitIterator("); show(io, itr.regex_and_state.pattern); print(io, ", "); show(io, itr.str); print(io, ')'))

function Base.iterate(itr::MatchSplitIterator, _ = nothing)
    regex_and_state = itr.regex_and_state
    state = regex_and_state.state
    e = itr.lastidx
    v_state = matchsplit_iterate!(regex_and_state.pattern, e, itr.str, state)
    isnothing(v_state) && return nothing
    v = first(v_state)
    return v, nothing
end

struct MatchSplits{P <: AbstractPattern, I <: MatchSplitIterator}
    regexes::Vector{P}
    str::SubString{String}
    states::MutableLinkedList{I}
    function MatchSplits(regexes::Vector{P}, str::SubString{String}) where P <:AbstractPattern
        n = length(regexes)
        @assert n != 0
        itr1 = MatchSplitIterator(@inbounds(regexes[1]), str)
        if P == AbstractPattern
            states = MutableLinkedList{MatchSplitIterator}(itr1)
        else
            states = MutableLinkedList{typeof(itr1)}(itr1)
        end
        return new{P, eltype(states)}(regexes, str, states)
    end
end
MatchSplits(regexes::Vector{<:AbstractPattern}, str::String) = MatchSplits(regexes, SubString(str))
MatchSplits(regexes, str) = MatchSplits(map(as_match, regexes), str)

Base.eltype(::Type{<:MatchSplits}) = Tuple{Bool, SubString{String}}
Base.IteratorSize(::Type{<:MatchSplits}) = Base.SizeUnknown()
Base.show(io::IO, itr::MatchSplits) = (print(io, "MatchSplits("); show(io, itr.regexes); print(io, ", "); show(io, itr.str); print(io, ')'))

function Base.iterate(itr::MatchSplits, _ = nothing)
    state = itr.states
    @label ms_itr_start
    level = length(state)
    iszero(level) && return nothing
    itr_i = @inbounds state[level]
    I = Base.iterate(itr_i)
    if isnothing(I)
        pop!(state)
        @goto ms_itr_start
    end
    v, _ = I
    ismatch = v[1]
    if ismatch
        return v, nothing
    else
        if level == length(itr.regexes)
            return v, nothing
        else
            regex_j = @inbounds itr.regexes[level+1]
            itr_j = MatchSplitIterator(regex_j, v[2])
            push!(state, itr_j)
            @goto ms_itr_start
        end
    end
end

matchsplit(t, s) = MatchSplitIterator(t, s)
matchsplits(t::AbstractPattern, s) = matchsplit(t, s)
matchsplits(t::Vector{<:AbstractPattern}, s) = isone(length(t)) ? matchsplits(@inbounds(t[1]), s) : MatchSplits(t, s)

"""
    matchsplits(pattern::AbstractPattern, str::String)

Split `str` with the regular expression `pattern`. Return a lazy iterator where each element
 is a `Tuple{Bool, SubString}`. The `Bool` indicate whether the `SubString` is a match of `pattern`.

# Example

```julia-repl
julia> matchsplits(r"a|c", "abc"^3)
MatchSplitIterator(r"a|c", "abcabcabc")

julia> collect(matchsplits(r"a|c", "abc"^3))
9-element Vector{Tuple{Bool, SubString{String}}}:
 (1, "a")
 (0, "b")
 (1, "c")
 (1, "a")
 (0, "b")
 (1, "c")
 (1, "a")
 (0, "b")
 (1, "c")

```
"""
matchsplits(t::AbstractPattern, s)

"""
    matchsplits(patterns::Vector{<:AbstractPattern}, str::String)

Split `str` with the list of regular expression `patterns`. Return a lazy iterator where each
 element is a `Tuple{Bool, SubString}`. The `Bool` indicate whether the `SubString` is a match of `pattern`.
 The match order are specified by the list order.

# Example

```julia-repl
julia> matchsplits([r"a", r"c"], "abc"^3)
MatchSplits(Regex[r"a", r"c"], "abcabcabc")

julia> collect(matchsplits([r"a", r"c"], "abc"^3))
9-element Vector{Tuple{Bool, SubString{String}}}:
 (1, "a")
 (0, "b")
 (1, "c")
 (1, "a")
 (0, "b")
 (1, "c")
 (1, "a")
 (0, "b")
 (1, "c")

julia> collect(matchsplits([r"ab", r"bc"], "abc"^3))
6-element Vector{Tuple{Bool, SubString{String}}}:
 (1, "ab")
 (0, "c")
 (1, "ab")
 (0, "c")
 (1, "ab")
 (0, "c")

```
"""
matchsplits(t::Vector{<:AbstractPattern}, s)

struct FindAllIterator{S, P}
    pattern::P
    str::S
end

Base.eltype(::Type{<:FindAllIterator{Union{String, SubString}}}) = SubString{String}
Base.eltype(::Type{<:FindAllIterator{AbstractString}}) = String
Base.IteratorSize(::Type{<:FindAllIterator}) = Base.SizeUnknown()

function Base.iterate(itr::FindAllIterator, state = firstindex(itr.str))
    str = itr.str
    found = findnext(itr.pattern, str, state)
    isnothing(found) && return nothing
    result = @inbounds eltype(itr) <: SubString ? @view(str[found]) : str[found]
    nstate = nextind(str, last(found))
    return result, nstate
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

const NotASample = -2
const UnknownSample = -1
const SampleElement = 0
const SingleSample = 1
const ArraySample = 2
const NestedSample = 3

_sequence_of(x) = x + (x >= 0)

"""
    type_sequence_sample_type([T::Type,] t::Type)

Get the depth of the nested array type. If return natural number, `t` is a type of nested array.
 Return `-1` if it cannot be known by type and return `-2` if `t` is not a nested array type.
 Specify `T` to check if `t` is a nested array type with element type `T`.
 If `T` is not specified, every type not subtype to `AbstractArray` is a count as element type.

see also: [`sequence_sample_type`](@ref), [`peek_sequence_sample_type`](@ref)

# Example

```julia-repl
julia> type_sequence_sample_type(Vector{Vector{Integer}})
2

julia> type_sequence_sample_type(Number, Array{Vector{Union{Float64, Int}}})
2

julia> type_sequence_sample_type(Int, Array{Vector{Union{Float64, Int}}})
-2

```
"""
function type_sequence_sample_type(@nospecialize(T::Type), @nospecialize(t::Type))
    t <: T && return SampleElement
    if t isa Union
        st_a = type_sequence_sample_type(T, t.a)
        st_b = type_sequence_sample_type(T, t.b)
        return st_a == st_b ? st_a : NotASample
    end
    t <: AbstractArray || return NotASample
    et = t >: AbstractArray ? Base.unwrap_unionall(t).parameters[1] : eltype(t)
    if et isa DataType || et isa UnionAll
        if et <: T
            return SingleSample
        elseif et <: AbstractArray
            st = type_sequence_sample_type(T, et)
            return _sequence_of(st)
        elseif et >: AbstractArray
            return UnknownSample
        else
            return NotASample
        end
    elseif et isa Union
        st = type_sequence_sample_type(T, et)
        return _sequence_of(st)
    end
    return UnknownSample
end
function type_sequence_sample_type(@nospecialize(t::Type))
    if t isa Union
        st_a = type_sequence_sample_type(t.a)
        st_b = type_sequence_sample_type(t.b)
        return st_a == st_b ? st_a : NotASample
    end
    t <: AbstractArray || return SampleElement
    et = t >: AbstractArray ? Base.unwrap_unionall(t).parameters[1] : eltype(t)
    if et isa DataType || et isa UnionAll
        if et <: AbstractArray
            st = type_sequence_sample_type(et)
            return _sequence_of(st)
        elseif et >: AbstractArray
            return UnknownSample
        else
            return SingleSample
        end
    elseif et isa Union
        st = type_sequence_sample_type(et)
        return _sequence_of(st)
    end
    return UnknownSample
end

"""
    sequence_sample_type([T::Type,] x)

Get the depth of the nested array. If return natural number, `x` is a nested array where each element has the same depth.
 Return `-2` if `x` is not a nested array or the depth of elements are different. Depth of empty array compute with the
 type and `sequence_sample_type(Any[])` is `1`. Specify `T` to check if `x` is a nested array with element type `T`.
 If `T` is not specified, every type not subtype to `AbstractArray` is a count as element type.

see also: [`type_sequence_sample_type`](@ref), [`peek_sequence_sample_type`](@ref)

# Example

```julia-repl
julia> sequence_sample_type([[1,2,3]])
2

julia> sequence_sample_type([[[2,3], [1]], Vector{Int}[]])
3

julia> sequence_sample_type([[[2,3], [1]], Any[]])
-2

julia> sequence_sample_type(Int, [[1,2], 3])
-2

julia> sequence_sample_type(Int, Any[[1,2], Int[]])
2

```
"""
function sequence_sample_type(x)
    S = typeof(x)
    stype = type_sequence_sample_type(S)
    if stype == UnknownSample
        itr = iterate(x)
        if !isnothing(itr)
            xi, state = itr
            elst = sequence_sample_type(xi)
            elst == NotASample && return NotASample
            itr = iterate(x, state)
            while !isnothing(itr)
                xi, state = itr
                elst2 = sequence_sample_type(xi)
                elst != elst2 && return NotASample
                itr = iterate(x, state)
            end
            return _sequence_of(elst)
        end
        ET = eltype(S)
        return ET <: AbstractArray || ET != Any ? ArraySample : SingleSample
    end
    return stype
end
function sequence_sample_type(T::Type, x)
    S = typeof(x)
    stype = type_sequence_sample_type(T, S)
    if stype == UnknownSample
        itr = iterate(x)
        if !isnothing(itr)
            xi, state = itr
            elst = sequence_sample_type(T, xi)
            elst == NotASample && return NotASample
            itr = iterate(x, state)
            while !isnothing(itr)
                xi, state = itr
                elst2 = sequence_sample_type(T, xi)
                elst != elst2 && return NotASample
                itr = iterate(x, state)
            end
            return _sequence_of(elst)
        end
        ET = eltype(S)
        return ET <: AbstractArray || ET != Any ? ArraySample : SingleSample
    end
    return stype
end

"""
    peek_sequence_sample_type([T::Type,] x)

Non-recursive version of `sequence_sample_type`. Return `-1` if the `x` is an array of array with unknown elements,
 thus it's possible that `sequence_sample_type(x[i]) == -2`. Specify `T` to check if `x` is a nested array with
 element type `T`. If `T` is not specified, every type not subtype to `AbstractArray` is a count as element type.

see also: [`type_sequence_sample_type`](@ref), [`sequence_sample_type`](@ref)

# Example

```julia-repl
julia> TextEncodeBase.peek_sequence_sample_type([1,2,3])
1

julia> peek_sequence_sample_type(Int, Any[[[1,2,3]]]), sequence_sample_type(Int, Any[[[1,2,3]]])
(-1, 3)

julia> peek_sequence_sample_type(Int, [[[1,2,3], "abc"]]), sequence_sample_type(Int, [[[1,2,3], "abc"]])
(-1, -2)

```
"""
function peek_sequence_sample_type(x)
    S = typeof(x)
    stype = type_sequence_sample_type(S)
    if stype == UnknownSample
        itr = iterate(x)
        if !isnothing(itr)
            xi, state = itr
            elst = xi isa AbstractArray ? SingleSample : SampleElement
            itr = iterate(x, state)
            while !isnothing(itr)
                xi, state = itr
                elst2 = xi isa AbstractArray ? SingleSample : SampleElement
                elst != elst2 && return NotASample
                itr = iterate(x, state)
            end
            return elst == SampleElement ? SingleSample : UnknownSample
        end
        ET = eltype(S)
        return ET <: AbstractArray || ET != Any ? ArraySample : SingleSample
    end
    return stype
end
function peek_sequence_sample_type(T::Type, x)
    S = typeof(x)
    stype = type_sequence_sample_type(T, S)
    if stype == UnknownSample
        itr = iterate(x)
        if !isnothing(itr)
            xi, state = itr
            elst = xi isa AbstractArray ? SingleSample : xi isa T ? SampleElement : NotASample
            elst == NotASample && return NotASample
            itr = iterate(x, state)
            while !isnothing(itr)
                xi, state = itr
                elst2 = xi isa AbstractArray ? SingleSample : xi isa T ? SampleElement : NotASample
                elst != elst2 && return NotASample
                itr = iterate(x, state)
            end
            return elst == SampleElement ? SingleSample : UnknownSample
        end
        ET = eltype(S)
        return ET <: AbstractArray || ET != Any ? ArraySample : SingleSample
    end
    return stype
end


macro elementmap(sym::Symbol, ex::Expr)
    !Meta.isexpr(ex, :call) && error("not a function call: $ex")
    func = ex.args[1]
    has_x = false
    argtype = Expr(:curly, :Tuple)
    x_i = Symbol("#", sym, :_i)
    fcall = Expr(:call, func)
    for i = 2:length(ex.args)
        argi = ex.args[i]
        if argi == sym
            has_x = true
            push!(argtype.args, :(eltype($argi)))
            push!(fcall.args, x_i)
        else
            push!(argtype.args, :(typeof($argi)))
            push!(fcall.args, argi)
        end
    end
    !has_x && error("no $sym in function call")
    f = Expr(:->, x_i, Expr(:block, fcall))
    ET = Expr(:call, :(Core.Compiler.return_type), func, argtype)
    RT = Expr(:curly, Array, ET, Expr(:call, :ndims, sym))
    y = Expr(:call, RT, :undef, Expr(:call, :size, sym))
    r = Expr(:call, :map!, f, y, sym)
    return esc(r)
end

allany(f, x) = mapfoldl(f, _allany, x; init=(true, false))
_allany(a, b) = a[1] & b, a[2] | b

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
function with_head_tail(x::AbstractArray, head, tail)
    stype = peek_sequence_sample_type(x)
    if stype == SingleSample
        T = eltype(x)
        if T == Any
            return _with_head_tail(mapreduce(typeof, promote_type, x), x, head, tail)
        else
            return _with_head_tail(x, head, tail)
        end
    elseif stype >= UnknownSample
        return @elementmap x with_head_tail(x, head, tail)
        # return map(FixRest(with_head_tail, head, tail), x)
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
function trunc_or_pad(x::AbstractArray, n::Integer, pad, trunc_end::Symbol = :tail, pad_end::Symbol = :tail)
    stype = peek_sequence_sample_type(x)
    if stype == SingleSample
        return trunc_or_pad!(similar(x, n), x, n, pad, trunc_end, pad_end)
    elseif stype >= UnknownSample
        return @elementmap x trunc_or_pad(x, n, pad, trunc_end, pad_end)
        # return map(trunc_or_pad(n, pad, trunc_end, pad_end), x)
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


function nestedmaxlength(x::AbstractArray)
    stype = peek_sequence_sample_type(x)
    if stype == SingleSample
        return length(x)
    elseif stype >= UnknownSample
        return mapfoldl(nestedmaxlength, max, x)
    else
        error("Input array is mixing array and non-array elements")
    end
end

_checkeqsize(x, y) = x == y ? x : throw(DimensionMismatch("nested size not the same: $x != $y"))
function nestedsize(x::AbstractArray)
    stype = peek_sequence_sample_type(x)
    if stype == SingleSample
        return size(x)
    elseif stype >= UnknownSample
        s1 = nestedsize(first(x))
        mapfoldl(nestedsize, _checkeqsize, @view(reshape(x, :)[2:end]); init = s1)
        return (s1..., size(x)...)
    else
        error("Input array is mixing array and non-array elements")
    end
end

function nestedtype(x::AbstractArray)
    stype = peek_sequence_sample_type(x)
    if stype == SingleSample
        return mapreduce(typeof, promote_type, x)
    elseif stype >= UnknownSample
        return mapreduce(nestedtype, promote_type, x)
    else
        error("Input array is mixing array and non-array elements")
    end
end

"""
    nested2batch(x)

convert nested array into single array

See also: [`batch2nested`](@ref)

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
    ns = nestedsize(x)
    arr = Array{nestedtype(x), length(ns)}(undef, ns)
    _nested2batch!(arr, 1, x)
    return arr
end

_reduce_nested(dst_offset, xi) = dst_offset[1], _nested2batch!(dst_offset..., xi)[2]
function _nested2batch!(arr, offset, x::AbstractArray)
    stype = peek_sequence_sample_type(x)
    if stype == SingleSample
        copyto!(arr, offset, x, 1, length(x))
        return (arr, offset+length(x))
    elseif stype >= UnknownSample
        return foldl(_reduce_nested, x; init=(arr, offset))
    else
        error("Input array is mixing array and non-array elements")
    end
end

"""
    batch2nested(x)

convert single array into nested array.

See also: [`nested2batch`](@ref)

# Example
```julia-repl
julia> x = ["a" "d"; "b" "e"; "c" "f";;; "x" "u"; "y" "v"; "z" "w"; ]
3×2×2 Array{String, 3}:
[:, :, 1] =
 "a"  "d"
 "b"  "e"
 "c"  "f"

[:, :, 2] =
 "x"  "u"
 "y"  "v"
 "z"  "w"

julia> TextEncodeBase.batch2nested(x)
2-element Vector{Vector{Vector{String}}}:
 [["a", "b", "c"], ["d", "e", "f"]]
 [["x", "y", "z"], ["u", "v", "w"]]

```
"""
function batch2nested(x::AbstractArray)
    return _batch2nested(x, size(x))
end

_batch2nested(x, ::Tuple{Int}) = collect(x)
@static if VERSION < v"1.9"
    function _batch2nested(x, s::Tuple)
        dim = length(s)
        len = s[end]
        s = Base.front(s)
        X = eachslice(x; dims = dim)
        y = Vector{Core.Compiler.return_type(_batch2nested, Tuple{eltype(X), typeof(s)})}(undef, len)
        @inbounds for (i, xi) in enumerate(X)
            y[i] = _batch2nested(xi, s)
        end
        return y
    end
else
    function _batch2nested(x, s::Tuple)
        dim = length(s)
        len = s[end]
        s = Base.front(s)
        X = eachslice(x; dims = dim)
        y = Vector{Core.Compiler.return_type(_batch2nested, Tuple{eltype(X), typeof(s)})}(undef, len)
        return map!(xi->_batch2nested(xi, s), y, X)
    end
end

"""
    join_text(x::AbstractArray [, delim [, last]])

`join` the inner most array and preserve the array structure. If the inner most array is multi-dimensional, `join`
 text along the first dimension.

# Example
```julia-repl
julia> TextEncodeBase.join_text([["a", "b", "c"], ['x', 'y', 'z']])
2-element Vector{String}:
 "abc"
 "xyz"

julia> TextEncodeBase.join_text([["a", "b", "c"], ['x', 'y', 'z']], " + ")
2-element Vector{String}:
 "a + b + c"
 "x + y + z"

julia> TextEncodeBase.join_text([[["a", "b", "c"], ['x', 'y', 'z']]], " + ", " = ")
1-element Vector{Vector{String}}:
 ["a + b = c", "x + y = z"]

julia> TextEncodeBase.join_text(["a" "d"; "b" "e"; "c" "f";;; "x" "u"; "y" "v"; "z" "w"; ], " + ", " = ")
2×2 Matrix{String}:
 "a + b = c"  "x + y = z"
 "d + e = f"  "u + v = w"

```
"""
@static if VERSION < v"1.9"
    function join_text(x::AbstractArray, delim = "", last = delim)
        stype = peek_sequence_sample_type(x)
        if stype == SingleSample
            N = ndims(x)
            if N == 1
                return join(x, delim, last)
            else
                return reshape(mapslices(FixRest(join, delim, last), x; dims = 1), Base.tail(size(x)))
            end
        elseif stype >= UnknownSample
            return @elementmap x join_text(x, delim, last)
        else
            error("Input array is mixing array and non-array elements")
        end
    end
else
    function join_text(x::AbstractArray, delim = "", last = delim)
        stype = peek_sequence_sample_type(x)
        if stype == SingleSample
            N = ndims(x)
            if N == 1
                return join(x, delim, last)
            else
                return map(FixRest(join, delim, last), eachslice(x, dims = ntuple(x->x+1, Val(N - 1))))
            end
        elseif stype >= UnknownSample
            return @elementmap x join_text(x, delim, last)
            # return map(FixRest(join_text, delim, last), x)
        else
            error("Input array is mixing array and non-array elements")
        end
    end
end


# Sequence template

"""
    abstract type TemplateTerm{T} end

Abstract type for term used in [`SequenceTemplate`](@ref).
"""
abstract type TemplateTerm{T} end

Base.eltype(::TemplateTerm{T}) where T = T

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
    RepeatedTerm(terms::TemplateTerm...; dynamic_type_id = false)

A special term that indicate the `terms` sequence can appear zero or multiple times. Cannot be nested.
 If `dynamic_type_id` is set, each repeat would add an offset value to the type id of those repeat `terms`.
 The offset value if the number of repetiton, starting form `0`, times `dynamic_type_id`.
"""
struct RepeatedTerm{T, Ts<:Tuple{Vararg{TemplateTerm{T}}}} <: TemplateTerm{T}
    terms::Ts
    dynamic_type_id::Int
    function RepeatedTerm(terms::Tuple{Vararg{TemplateTerm{T}}}, dynamic_type_id = false) where T
        @assert length(terms) >= 1 "No TemplateTerm provided."
        @assert !any(Base.Fix2(isa, RepeatedTerm), terms) "Cannot nest RepeatedTerm"
        return new{T, typeof(terms)}(terms, dynamic_type_id)
    end
end
RepeatedTerm(terms::TemplateTerm...; dynamic_type_id = false) = RepeatedTerm(terms, dynamic_type_id)

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

Base.eltype(::SequenceTemplate{T}) where T = T

function process_term!(term::InputTerm, output, type_ids, i, j, terms, xs)
    @assert j <= length(xs) "InputTerm indexing $j-th input but only get $(length(xs))"
    x = xs[j]
    isnothing(output)   || append!(output, x)
    isnothing(type_ids) || append!(type_ids, Iterators.repeated(term.type_id, length(x)))
    return j + 1
end

function process_term!(term::IndexInputTerm, output, type_ids, i, j, terms, xs)
    idx = term.idx
    @assert idx <= length(xs) "IndexInputTerm indexing $idx-th input but only get $(length(xs))"
    x = xs[idx]
    isnothing(output)   || append!(output, x)
    isnothing(type_ids) || append!(type_ids, Iterators.repeated(term.type_id, length(x)))
    return idx == j ? j + 1 : j
end

function process_term!(term::ConstTerm, output, type_ids, i, j, terms, xs)
    isnothing(output)   || push!(output, term.value)
    isnothing(type_ids) || push!(type_ids, term.type_id)
    return j
end

function process_term!(term::RepeatedTerm, output, type_ids, i, j, terms, xs)
    r_terms = term.terms
    dynamic_type_id = term.dynamic_type_id
    n = count(Base.Fix2(isa, InputTerm), terms[i+1:end])
    J = length(xs) - n
    type_id_offset = 0
    while j <= J
        if !isnothing(type_ids)
            type_id_start = length(type_ids) + 1
        end

        _j = j
        for (t_i, term_i) in enumerate(r_terms)
            j = process_term!(term_i, output, type_ids, t_i, j, r_terms, xs)
        end
        _j == j && error("RepeatedTerm doesn't seem to terminate")

        if !isnothing(type_ids)
            type_id_end = length(type_ids)
            dynamic_type_id != 0 && (type_ids[type_id_start:type_id_end] .+= type_id_offset)
            type_id_offset += dynamic_type_id
        end
    end
    return j
end

function process_template!(
    st::SequenceTemplate{T}, output::Union{Vector{T}, Nothing}, type_ids::Union{Vector{Int}, Nothing}, xs
) where T
    terms = st.terms
    len = length(xs)
    n_input = count(Base.Fix2(isa, InputTerm), terms)
    @assert len >= n_input "SequenceTemplate require at least $n_input but only get $len"

    j = 1
    for (i, term) in enumerate(terms)
         j = process_term!(term, output, type_ids, i, j, terms, xs)
    end
    @assert j > len "SequenceTemplate only take $(j-1) inputs but get $len"

    return output, type_ids
end

alloc_outputs(st::SequenceTemplate, ::Val{0}) = (Vector{eltype(st)}(), Vector{Int}())
alloc_outputs(st::SequenceTemplate, ::Val{1}) = (Vector{eltype(st)}(), nothing)
alloc_outputs(st::SequenceTemplate, ::Val{2}) = (nothing, Vector{Int}())
alloc_outputs(st::SequenceTemplate, ::Val{-1}) = (nothing, nothing)

apply_template(st::SequenceTemplate) = Base.Fix1(apply_template, st)
apply_template(st::SequenceTemplate, val::Val) = xs -> apply_template(st, val, xs)
apply_template(st::SequenceTemplate, xs) = apply_template(st, Val(0), xs)
apply_template(st::SequenceTemplate, val::Val, xs) = apply_template!(st, alloc_outputs(st, val), xs)

function apply_template!(st::SequenceTemplate, buffers::Tuple{A, B}, xs) where {A, B}
    output, type_ids = process_template!(st, buffers[1], buffers[2], xs)
    if !(isnothing(output) || isnothing(type_ids))
        return output, type_ids
    elseif !isnothing(output)
        return output
    elseif !isnothing(type_ids)
        return type_ids
    else
        return nothing
    end
end

(st::SequenceTemplate)(val::Val) = Base.Fix1(st, val)
(st::SequenceTemplate)(x::AbstractArray) = st(Val(0), x)
(st::SequenceTemplate{T})(xs::AbstractVector{T}...) where T = st(xs)
(st::SequenceTemplate{T})(xs::Tuple{Vararg{AbstractVector{T}}}) where T = st(Val(0), xs)

## static single sample
(st::SequenceTemplate{T})(val::Val, x::AbstractVector{T}, xs::AbstractVector{T}...) where T = apply_template(st, val, (x, xs...))
(st::SequenceTemplate{T})(val::Val, xs::Tuple{Vararg{AbstractVector{T}}}) where T = apply_template(st, val, xs)
(st::SequenceTemplate{T})(val::Val, xs::AbstractVector{<:AbstractVector{T}}) where T = apply_template(st, val, xs)

## static multiple sample
function (st::SequenceTemplate{T})(val::Val, xs::AbstractArray{<:AbstractVector{<:AbstractVector{T}}}) where T
    if val == Val(0)
        outputs = map(apply_template(st, Val(1)), xs)
        type_ids = map(apply_template(st, Val(2)), xs)
        return outputs, type_ids
    elseif val == Val(-1)
        foreach(apply_template(st, Val(-1)), xs)
        return nothing
    else
        return map(apply_template(st, val), xs)
    end
end

## deep nested or dynamic
@inline function _st_call(st::SequenceTemplate, val::Val, xs::AbstractArray)
    return @elementmap xs st(val)(xs)
end

@inline function _st_nested(st::SequenceTemplate, val::Val, xs::AbstractArray)
    if val == Val(0)
        outputs = _st_call(st, Val(1), xs)
        type_ids = _st_call(st, Val(2), xs)
        return outputs, type_ids
    elseif val == Val(-1)
        foreach(st(Val(-1)), xs)
        return nothing
    else
        return _st_call(st, val, xs)
    end
end

function (st::SequenceTemplate{T})(val::Val, xs::AbstractArray) where T
    if isnestedconcretetype(typeof(xs))
        return _st_nested(st, val, xs)
    end
    aoa, naov = allany(Base.Fix2(isa, AbstractArray), xs)
    aov = !naov
    if aoa
        if all(Base.Fix1(all, Base.Fix2(isa, T)), xs) # dynamic single sample
            # xs is an array of sequence
            return apply_template(st, val, xs)
        elseif all(Base.Fix1(all, Base.Fix2(isa, AbstractArray)), xs) # dynamic multiple sample
            # xs is an array of array of array
            # return map(st(val), xs)
            if val == Val(0)
                outputs = map(st(Val(1)), xs)
                type_ids = map(st(Val(2)), xs)
                return outputs, type_ids
            elseif val == Val(-1)
                foreach(st(Val(-1)), xs)
                return nothing
            else
                return map(st(val), xs)
            end
            # return _st_nested(st, val, xs)
        else
            throw(MethodError(st, xs))
        end
    elseif aov # dynamic single sample
        # xs is a sequence
        !all(Base.Fix2(isa, T), xs) && throw(MethodError(st, xs)) # assert eltype of sequence == T
        return apply_template(st, val, (xs,))
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
    if iszero(t.dynamic_type_id)
        print(io, ")...")
    else
        print(io, ")<type+=$(t.dynamic_type_id)>...")
    end
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

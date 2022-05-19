struct Pipeline{name, F}
    f::F
    function Pipeline{name, F}(f::F) where {name, F}
        name isa Symbol || name isa NTuple{N, Symbol} where N && !(name isa Tuple{}) ||
            error("Pipeline name must be a Symbol or Tuple of Symbol: get $name")
        return new{name, F}(f)
    end
end
Pipeline{name}(f) where name = Pipeline{name, typeof(f)}(f)
Pipeline{name}(f::ApplyN{N}) where {name, N} = (N == 1 || N == 2) ? Pipeline{name, typeof(f)}(f) : error("attempt to access $n-th argument while pipeline only take 2")

Pipeline{name}(f, n::Int) where name = Pipeline{name}(ApplyN{n}(f))
Pipeline{name}(f, syms::Union{Symbol, Tuple{Vararg{Symbol}}}) where name = Pipeline{name}(ApplySyms{syms}(f), 2)

_name(p::Pipeline{name}) where name = name

@inline _result_namedtuple(p::Pipeline, result) = _result_namedtuple(_name(p), result)
@inline _result_namedtuple(name::Symbol, result) = _result_namedtuple((name,), result)
@inline _result_namedtuple(name::NTuple{N, Symbol} where N, result) = NamedTuple{name}((result,))
@inline _result_namedtuple(name::NTuple{N, Symbol} where N, result::Tuple) = NamedTuple{name}(result)

(p::Pipeline{name})(x, y = NamedTuple()) where name = merge(y, _result_namedtuple(p, p.f(x, y)))

struct Pipelines{T<:NTuple{N, Pipeline} where N}
    pipes::T
end
Pipelines{Tuple{}}(::Tuple{}) = error("empty pipelines")
Pipelines(p1, ps...) = Pipelines((p1, ps...))

Base.length(ps::Pipelines) = length(ps.pipes)
Base.iterate(ps::Pipelines, state=1) = iterate(ps.pipes, state)
Base.firstindex(ps::Pipelines) = firstindex(ps.pipes)
Base.lastindex(ps::Pipelines) = lastindex(ps.pipes)

@inline Base.getindex(ps::Pipelines, i) = ps.pipes[i]

function (ps::Pipelines{T})(x) where T
    if @generated
        body = Expr[ :(y = ps[$n](x, y)) for n = 1:fieldcount(T)]
        return quote
            y = NamedTuple()
            $(body...)
        end
    else
        foldl((y, p)->p(x, y), ps.pipes; init=NamedTuple())
    end
end

Base.:(|>)(p1::Pipeline, p2::Pipeline) = Pipelines(p1, p2)
Base.:(|>)(p1::Pipelines, p2::Pipeline) = Pipelines(p1.pipes..., p2)
Base.:(|>)(p1::Pipeline, p2::Pipelines) = Pipelines(p1, p2.pipes...)
Base.:(|>)(p1::Pipelines, p2::Pipelines) = Pipelines(p1.pipes..., p2.pipes...)

function __getindex__ end

"""
    PipeGet{name}()

A special pipeline that get the wanted `name`s from namedtuple.

# Example

```julia
julia> p = Pipeline{:x}(identity, 1) |> Pipeline{(:sinx, :cosx)}(sincos, 1) |> PipeGet{(:x, :sinx)}()
Pipelines: Pipeline{x}((x,_)->identity(x)) => Pipeline{(:sinx, :cosx)}((x,_)->sincos(x)) => Pipeline{(:x, :sinx)}(__getindex__)

julia> p(0.5)
(x = 0.5, sinx = 0.479425538604203)

julia> p = Pipeline{:x}(identity, 1) |> Pipeline{(:sinx, :cosx)}(sincos, 1) |> PipeGet{:sinx}()
Pipelines: Pipeline{x}((x,_)->identity(x)) => Pipeline{(:sinx, :cosx)}((x,_)->sincos(x)) => Pipeline{(:x, :sinx)}(__getindex__)

julia> p(0.5)
0.479425538604203

```

"""
const PipeGet{name} = Pipeline{name, typeof(__getindex__)}

PipeGet{name}() where name = PipeGet{name}(__getindex__)

@static if VERSION < v"1.7"
    (p::PipeGet{name})(_, y) where name = name isa Symbol ? y[name] : NamedTuple{name}(y)
else
    (p::PipeGet{name})(_, y) where name = y[name]
end


"""
    Pipeline{name}(f)

Create a pipeline function with name. When calling the pipeline function, mark the result with `name`.
 `f` should take two arguemnt: the input and a namedtuple (can be ignored) that the result will be
 merged to. `name` can be either `Symbol` or tuple of `Symbol`s.


    Pipeline{name}(f, n) : equivalent to Pipeline{name}((args...)->f(args[n]))

Create a pipline function with name. `f` should take one argument, it will be applied to either
 the input or namedtuple depend on the value of `n`. `n` should be either `1` or `2`.

# Example

```julia
julia> p = Pipeline{:x}(1) do x
           2x
       end
Pipeline{x}((x,_)->#27(x))

julia> p(3)
(x = 6,)

julia> p = Pipeline{:x}() do x, y
           y.a * x
       end
Pipeline{x}(#31)

julia> p(2, (a=3, b=5))
(a = 3, b = 5, x = 6)

julia> p = Pipeline{:x}(y->y.a^2, 2)
Pipeline{x}((_,y)->#29(y))

julia> p(2, (a = 3, b = 5))
(a = 3, b = 5, x = 9)

julia> p = Pipeline{(:sinx, :cosx)}(sincos, 1)
Pipeline{(:sinx, :cosx)}((x,_)->sincos(x))

julia> p(0.5)
(sinx = 0.479425538604203, cosx = 0.8775825618903728)

```

"""
Pipeline

"""
    Pipelines(pipeline...)

Chain of `Pipeline`s.

# Example

```julua
julia> pipes = Pipelines(Pipeline{:x}((x,y)->x), Pipeline{(:sinx, :cosx)}((x,y)->sincos(x)))
Pipelines: Pipeline{x}(#25) => Pipeline{(:sinx, :cosx)}(#26)

julia> pipes(0.3)
(x = 0.3, sinx = 0.29552020666133955, cosx = 0.955336489125606)

# or use `|>`
julia> pipes = Pipeline{:x}((x,y)->x) |> Pipeline{(:sinx, :cosx)}((x,y)->sincos(x))
Pipelines: Pipeline{x}(#25) => Pipeline{(:sinx, :cosx)}(#26)

julia> pipes(0.3)
(x = 0.3, sinx = 0.29552020666133955, cosx = 0.955336489125606)

```
"""
Pipelines

function Base.show(io::IO, p::Pipeline)
    print(io, "Pipeline{$(_name(p))}(")
    if p.f isa ApplyN
        n = _nth(p.f)
        if n == 1
            f = "(x,_)->$(p.f.f)(x)"
        elseif n == 2
            f = "(_,y)->$(p.f.f)(y)"
        else
            f = "$(p.f)"
        end
    else
        f = "$(p.f)"
    end
    print(io, f)
    print(io, ')')
end

Base.show(io::IO, ps::Pipelines) = (print(io, "Pipelines: "); join(io, ps.pipes, " => "))

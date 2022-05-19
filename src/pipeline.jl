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

"""
    PipeGet{name}()

A special pipeline that get the wanted `name`s from namedtuple.

# Example

```julia
julia> p = Pipeline{:x}(identity, 1) |> Pipeline{(:sinx, :cosx)}(sincos, 1) |> PipeGet{(:x, :sinx)}()
Pipelines:
  target[x] := identity(source)
  target[(sinx, cosx)] := sincos(source)
  target := (target.x, target.sinx)

julia> p(0.5)
(x = 0.5, sinx = 0.479425538604203)

julia> p = Pipeline{:x}(identity, 1) |> Pipeline{(:sinx, :cosx)}(sincos, 1) |> PipeGet{:sinx}()
Pipelines:
  target[x] := identity(source)
  target[(sinx, cosx)] := sincos(source)
  target := (target.sinx)

julia> p(0.5)
0.479425538604203

```

"""
const PipeGet{name} = Pipeline{name, typeof(__getindex__)}

PipeGet{name}() where name = PipeGet{name}(__getindex__)
(p::PipeGet{name})(_, y) where name = __getindex__(y, name)


"""
    Pipeline{name}(f)

Create a pipeline function with name. When calling the pipeline function, mark the result with `name`.
 `f` should take two arguemnt: the input and a namedtuple (can be ignored) that the result will be
 merged to. `name` can be either `Symbol` or tuple of `Symbol`s.


    Pipeline{name}(f, n)

Create a pipline function with name. `f` should take one argument, it will be applied to either the input
 or namedtuple depend on the value of `n`. `n` should be either `1` or `2`. Equivalent to
 `f(n == 1 ? source : target)`.

    Pipeline{name}(f, syms)

Create a pipline function with name. `syms` can be either a `Symbol` or a tuple of `Symbol`s.
 Equivalent to `f(target[syms])` or `f(target[syms]...)` depends on the type of `syms`.

# Example

```julia
julia> p = Pipeline{:x}(1) do x
           2x
       end
Pipeline{x}(var"#19#20"()(source))

julia> p(3)
(x = 6,)

julia> p = Pipeline{:x}() do x, y
           y.a * x
       end
Pipeline{x}(var"#21#22"()(source, target))

julia> p(2, (a=3, b=5))
(a = 3, b = 5, x = 6)

julia> p = Pipeline{:x}(y->y.a^2, 2)
Pipeline{x}(var"#23#24"()(target))

julia> p(2, (a = 3, b = 5))
(a = 3, b = 5, x = 9)

julia> p = Pipeline{(:sinx, :cosx)}(sincos, 1)
Pipeline{(sinx, cosx)}(sincos(source))

julia> p(0.5)
(sinx = 0.479425538604203, cosx = 0.8775825618903728)

julia> p = Pipeline{:z}((x, y)-> 2x+y, (:x, :y))
Pipeline{z}(var"#33#34"()(target.x, target.y))

julia> p(0, (x=3, y=5))
(x = 3, y = 5, z = 11)

```

"""
Pipeline

"""
    Pipelines(pipeline...)

Chain of `Pipeline`s.

# Example

```julua
julia> pipes = Pipelines(Pipeline{:x}((x,y)->x), Pipeline{(:sinx, :cosx)}((x,y)->sincos(x)))
Pipelines:
  target[x] := var"#25#27"()(source, target)
  target[(sinx, cosx)] := var"#26#28"()(source, target)

julia> pipes(0.3)
(x = 0.3, sinx = 0.29552020666133955, cosx = 0.955336489125606)

# or use `|>`
julia> pipes = Pipeline{:x}((x,y)->x) |> Pipeline{(:sinx, :cosx)}((x,y)->sincos(x))
Pipelines:
  target[x] := var"#29#31"()(source, target)
  target[(sinx, cosx)] := var"#30#32"()(source, target)

julia> pipes(0.3)
(x = 0.3, sinx = 0.29552020666133955, cosx = 0.955336489125606)

```
"""
Pipelines

# display
@nospecialize

function show_pipeline_function(io::IO, f1::Base.Fix1)
    print(io, "(x->")
    show_pipeline_function(io, f1.f)
    print(io, '(', f1.x, ", x))")
end
function show_pipeline_function(io::IO, f2::Base.Fix2)
    print(io, "(x->")
    show_pipeline_function(io, f2.f)
    print(io, "(x, ", f2.x, "))")
end
function show_pipeline_function(io::IO, a::ApplyN)
    print(io, "(args...->")
    show_pipeline_function(io, a.f)
    print(io, "(args[", _nth(a), "]))")
end
function show_pipeline_function(io::IO, a::ApplySyms)
    print(io, "((; kwargs...)->")
    show_pipeline_function(io, a.f)
    print(io, "(kwargs[", '(', _syms(a), ')', "]...))")
end
function show_pipeline_function(io::IO, fr::FixRest)
    show_pipeline_function(io, fr.f)
    print(io, '(')
    join(io, fr.arg, ", ")
    print(io, ')')
end
function show_pipeline_function(io::IO, c::ComposedFunction, nested=false)
    if nested
        show_pipeline_function(io, c.outer, nested)
        print(io, " ∘ ")
        show_pipeline_function(io, c.inner, nested)
    else
        print(io, '(', sprint(show_pipeline_function, c, true), ')')
    end
end
show_pipeline_function(io::IO, f, _) = show_pipeline_function(io, f)
show_pipeline_function(io::IO, f) = print(io, f)

function _show_pipeline_fixf(io::IO, g, name)
    if g isa Base.Fix1
        print(io, g.f, '(', g.x, ", ", name, ')')
    elseif g isa Base.Fix2
        print(io, g.f, '(', name, ", ", g.x, ')')
    else
        show_pipeline_function(io, g)
        print(io, '(', name, ')')
    end
end

function show_pipeline_function(io::IO, p::Pipeline)
    if p.f isa ApplyN
        n = _nth(p.f)
        g = p.f.f
        if n == 1
            _show_pipeline_fixf(io, g, :source)
        elseif n == 2
            if g isa ApplySyms
                syms = _syms(g)
                _show_pipeline_fixf(io, g.f, syms isa Tuple ? join(map(x->"target.$x", syms), ", ") : "target.$syms")
            else
                _show_pipeline_fixf(io, g, :target)
            end
        else
            print(io, p.f)
        end
    else
        _show_pipeline_fixf(io, p.f, "source, target")
    end
end

function show_pipeline_function(io::IO, p::PipeGet)
    name = _name(p)
    if name isa Tuple
        print(io, "(target.")
        join(io, name, ", target.")
        print(io, ')')
    else
        print(io, "(target.$name)")
    end
end

function Base.show(io::IO, p::Pipeline)
    print(io, "Pipeline{")
    name = _name(p)
    name isa Tuple ? (print(io, '('); join(io, name, ", "); print(io, ')')) : print(io, name)
    print(io, "}(")
    show_pipeline_function(io, p)
    print(io, ')')
end

function show_pipeline(io::IO, ps::Pipelines; flat=false, prefix=nothing)
    print(io, "Pipelines")
    flat || print(io, ":\n")
    n = length(ps.pipes)
    sprefix = isnothing(prefix) ? "  " : "$prefix"
    flat && print(io, '(')
    for (i, p) in enumerate(ps.pipes)
        flat || print(io, sprefix)

        if p isa PipeGet
            print(io, "target := ")
            show_pipeline_function(io, p)
        else
            print(io, "target[")
            name = _name(p)
            if name isa Symbol
                print(io, name)
            else
                print(io, '(')
                join(io, name, ", ")
                print(io, ')')
            end
            print(io, "] := ")
            show_pipeline_function(io, p)
        end

        if i != n
            flat ? print(io, "; ") : print(io, '\n')
        end
    end
    flat && print(io, ')')
end

function Base.show(io::IO, ps::Pipelines)
    prefix = get(io, :pipeline_display_prefix, nothing)
    flat = get(io, :compact, false)
    show_pipeline(io, ps; flat, prefix)
end

@specialize

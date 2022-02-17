struct Pipeline{name, F}
    f::F
    function Pipeline{name, F}(f::F) where {name, F}
        name isa Symbol || name isa NTuple{N, Symbol} where N && !(name isa Tuple{}) ||
            error("Pipeline name must be a Symbol or Tuple of Symbol: get $name")
        return new{name, F}(f)
    end
end
Pipeline{name}(f) where name = Pipeline{name, typeof(f)}(f)

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

(ps::Pipelines)(x) = foldl((y, p)->p(x, y), ps.pipes; init=NamedTuple())

"""
    Pipeline{name}(f)

Create a pipeline function with name. When calling the pipeline function, mark the result with `name`.
 `f` should take two arguemnt: the input and a namedtuple (can be ignored) that the result will be
 merged to. `name` can be either `Symbol` or tuple of `Symbol`s.

# Example

```julia
julia> p = Pipeline{:x}() do x, _
           2x
       end
Pipeline{:x, var"#7#8"}(var"#7#8"())

julia> p(3)
(x = 6,)

julia> p = Pipeline{:x}() do x, y
           y.a * x
       end
Pipeline{:x, var"#9#10"}(var"#9#10"())

julia> p(2, (a=3, b=5))
(a = 3, b = 5, x = 6)

julia> p = Pipeline{(:sinx, :cosx)}((x,y)->sincos(x))
Pipeline{(:sinx, :cosx), var"#15#16"}(var"#15#16"())

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
Pipelines{Tuple{Pipeline{:x, var"#11#13"}, Pipeline{(:sinx, :cosx), var"#12#14"}}}((Pipeline{:x, var"#11#13"}(var"#11#13"()), Pipeline{(:sinx, :cosx), var"#12#14"}(var"#12#14"())))

julia> pipes(0.3)
(x = 0.3, sinx = 0.29552020666133955, cosx = 0.955336489125606)

```
"""
Pipelines

Base.show(io::IO, p::Pipeline) = print(io, "Pipeline{$(_name(p))}($(p.f))")
Base.show(io::IO, ps::Pipelines) = (print(io, "Pipelines: "); join(io, ps.pipes, " => "))

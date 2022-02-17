using Base.Meta: isexpr

function stagem(sig, abst)
    abst = esc(abst)
    if sig isa Symbol
        name = sig
        structdef = :(
            struct $name{T, M} <: $abst
              x::T
              meta::M
            end
        )
    elseif sig isa Expr
        if isexpr(sig, :curly)
            length(sig.args) != 3 && error("invalid TokenStages definition")
            name, Tsig, Msig = sig.args
            T = isexpr(Tsig, :<:) ? Tsig.args[1] : Tsig
            M = isexpr(Msig, :<:) ? Msig.args[1] : Msig
            structdef = :(
                struct $sig <: $abst
                  x::$T
                  meta::$M
                end
            )
        else
            error("invalid TokenStages definition")
        end
    else
        error("invalid TokenStages definition")
    end
    name = esc(name)
    setmeta = esc(:(TextEncodeBase.setmeta))
    setvalue = esc(:(TextEncodeBase.setvalue))
    return quote
        $structdef
        $name(x) = $name(x, nothing)
        $setmeta(x::$name, meta) = $name(x.x, meta)
        $setvalue(x::$name, y) = $name(y, x.meta)
    end
end

"""
    @stage StageName
    @stage StageName{A<:SomeType, B}
    @stage StageName AbstractStage
    @stage StageName{A<:SomeType, B} <: AbstractStage

Define `TokenStages` with two field (`x` and `meta`), it's single arguement constructor,
 and add methods to `setmeta` and `setvalue`.

Equivalent to:

```julia
struct StageName{A<:SomeType, B} <: AbstractStage
    x::A
    meta::B
end

StageName(x) = StageName(x, nothing)
TextEncodeBase.setmeta(x::StageName, meta) = StageName(x.x, meta)
TextEncodeBase.setvalue(x::StageName, y) = StageName(y, x.meta)

```
"""
macro stage(sig, abst=:TokenStages)
    if isexpr(sig, :<:)
        abst != :TokenStages && error("invalid TokenStages definition")
        sig, abst = sig.args
    end
    return stagem(sig, abst)
end

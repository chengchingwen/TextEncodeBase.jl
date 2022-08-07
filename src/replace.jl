using StructWalk
using StructWalk: WalkStyle

struct TokenizerStyle <: WalkStyle end

StructWalk.children(::TokenizerStyle, x) = ()
StructWalk.iscontainer(::TokenizerStyle, x) = false

StructWalk.children(::TokenizerStyle, x::AbstractTokenizer) = StructWalk.children(WalkStyle, x)
StructWalk.children(::TokenizerStyle, x::AbstractTokenization) = StructWalk.children(WalkStyle, x)

Base.replace(f::Function, x::Union{AbstractTokenizer, AbstractTokenization}) = postwalk(f, TokenizerStyle(), x)

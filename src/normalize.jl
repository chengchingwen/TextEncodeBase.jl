abstract type TextNormalizer{T<:AbstractTokenization} <: WrappedTokenization{T} end
abstract type SentenceNormalizer{T<:AbstractTokenization} <: TextNormalizer{T} end
abstract type WordNormalizer{T<:AbstractTokenization} <: TextNormalizer{T} end

function normalizer end

# perform normalization on sentence level.
splitting(p::ParentStages, t::SentenceNormalizer, x::Union{SentenceStage, SubSentenceStage}) = splitting(p, base(t), updatevalue(normalizer(t), x))

# directly passing unsplittable should also be normalized, except token.
wrap(::Nothing, t::SentenceNormalizer, s::TokenStages) = wrap(nothing, base(t), updatevalue(normalizer(t), s))
wrap(::Nothing, t::SentenceNormalizer, s::TokenStage) = wrap(nothing, base(t), s)

# perform normalization on  word level.
wrap(p::TokenStages, t::WordNormalizer, s::WordStage) = wrap(p, base(t), updatevalue(normalizer(t), s))
# if word is splitable
splitting(p::ParentStages, t::WordNormalizer, x::WordStage) = splitting(p, base(t), updatevalue(normalizer(t), x))

# directly passing unsplittable should also be normalized, except token.
wrap(::Nothing, t::WordNormalizer, s::TokenStages) = wrap(nothing, base(t), updatevalue(normalizer(t), s))
wrap(::Nothing, t::WordNormalizer, s::TokenStage) = wrap(nothing, base(t), s)

### lower case

struct LowercaseNormalizer{T<:AbstractTokenization} <: SentenceNormalizer{T}
    base::T
end
LowercaseNormalizer() = LowercaseNormalizer(DefaultTokenization())

normalizer(t::LowercaseNormalizer) = lowercase

### Unicode

include("./unicode.jl")

struct UnicodeNormalizer{T<:AbstractTokenization} <: SentenceNormalizer{T}
    base::T
    flags::Int
    UnicodeNormalizer(base::AbstractTokenization, normalform::Symbol) = new{typeof(base)}(base, _utf8proc_flags(normalform))
    UnicodeNormalizer(base::AbstractTokenization; kw...) = new{typeof(base)}(base, _utf8proc_flags(; kw...))
end
UnicodeNormalizer(normalform::Symbol) = UnicodeNormalizer(DefaultTokenization(), normalform)
UnicodeNormalizer(; kw...) = UnicodeNormalizer(DefaultTokenization(); kw...)

normalizer(t::UnicodeNormalizer) = Base.Fix2(utf8proc_map, t.flags)

function Base.show(io::IO, t::UnicodeNormalizer)
    nfs = (:NFC, :NFD, :NFKC, :NFKD)
    idx = findfirst(==(t.flags), map(_utf8proc_flags, nfs))
    if isnothing(idx)
        print(io, "UnicodeNormalizer(")
        show(io, base(t))
        _show_utf8proc_flags(io, t.flags)
        print(io, ')')
    else
        name = nfs[idx]
        print(io, name, '(')
        show(io, base(t))
        print(io, ')')
    end
end

### Codemap

include("./codemap.jl")

struct CodeNormalizer{T<:AbstractTokenization, C <: CodeMap} <: WordNormalizer{T}
    base::T
    codemap::C
end
CodeNormalizer(codemap::CodeMap) = CodeNormalizer(DefaultTokenization(), codemap)
CodeNormalizer(base::AbstractTokenization, code_range, code_ranges...) = CodeNormalizer(base, CodeMap(code_range, code_ranges...))
CodeNormalizer(code_range, code_ranges...) = CodeNormalizer(CodeMap(code_range, code_ranges...))

TextEncodeBase.normalizer(t::CodeNormalizer) = t.codemap

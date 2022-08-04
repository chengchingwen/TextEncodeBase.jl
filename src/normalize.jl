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
wrap(p::ParentStages, t::WordNormalizer, s::WordStage) = wrap(p, base(t), updatevalue(normalizer(t), s))
# if word is splitable
splitting(p::ParentStages, t::WordNormalizer, x::WordStage) = splitting(p, base(t), updatevalue(normalizer(t), x))

# directly passing unsplittable should also be normalized, except token.
wrap(::Nothing, t::WordNormalizer, s::TokenStages) = wrap(nothing, base(t), updatevalue(normalizer(t), s))
wrap(::Nothing, t::WordNormalizer, s::TokenStage) = wrap(nothing, base(t), s)

### lower case

struct LowercaseNormalizer{T} <: SentenceNormalizer{T}
    base::T
end
LowercaseNormalizer() = LowercaseNormalizer(DefaultTokenization())

normalizer(t::LowercaseNormalizer) = lowercase

### Unicode

include("./unicode.jl")

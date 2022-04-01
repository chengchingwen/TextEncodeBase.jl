using Base.Iterators: repeated

mutable struct Offsets
    word::Int
    token::Int
end

struct IndexedTokenization{T<:AbstractTokenization} <: WrappedTokenization{T}
    base::T
end
IndexedTokenization() = IndexedTokenization(DefaultTokenization())

_offsets(s, w=0, t=0) = (meta = getmeta(s); hasmeta(s) && haskey(getmeta(s), :offsets) ? meta.offsets : Offsets(w,t))

@inline splitting(p::ParentStages, t::IndexedTokenization, s::TokenStages, x) = zip(repeated(_offsets(s)), Iterators.Flatten((true, Iterators.repeated(false))), splitting(p, t.base, s, x))
@inline splitting(p::ParentStages, t::IndexedTokenization, d::DocumentStage, x) = enumerate(splitting(p, t.base, d, x))

@inline wrap(p::ParentStages, t::IndexedTokenization, s::TokenStages, (i, f, x)) = updatemeta(wrap(p, t.base, s, x), (offsets = i, isfirst = f))
@inline wrap(p::ParentStages, t::IndexedTokenization, d::DocumentStage, (i, x)) = updatemeta(wrap(p, t.base, d, x), (sentence_id = i,))

@inline wrap(p::ParentStages, t::IndexedTokenization, s::TokenStages) = wrap(p, t.base, s)

function wrap(p::ParentStages, t::IndexedTokenization, w::SubWordStage)
    meta = getmeta(w)
    if hasmeta(w) && haskey(meta, :offsets)
        offsets = meta.offsets
        word_id = meta.isfirst ? (offsets.word += 1) : offsets.word
    else
        word_id = 1
    end
    return updatemeta(wrap(p, t.base, w), (word_id = word_id,))
end

function wrap(p::ParentStages, t::IndexedTokenization, x::TokenStage)
    x = wrap(p, t.base, x)
    meta = getmeta(x)
    if hasmeta(x) && haskey(meta, :offsets)
        offsets = meta.offsets
        word_id = haskey(meta, :word_id) ? meta.word_id : (offsets.word += 1)
        token_id = offsets.token += 1
        meta = Base.structdiff(meta, NamedTuple{(:offsets, :isfirst)})
    else
        word_id = token_id = 1
    end
    return setmeta(x, updatemeta(meta, (word_id = word_id, token_id = token_id)))
end

mutable struct Offsets
    word::Int
    token::Int
end

using Base.Iterators: repeated

struct IndexedTokenization <: AbstractTokenization end

_offsets(s) = (meta = getmeta(s); hasmeta(s) && haskey(getmeta(s), :offsets) ? meta.offsets : Offsets(0,0))

@inline splitting(::IndexedTokenization, s::TokenStages, x) = zip(repeated(_offsets(s)), x)
@inline splitting(::IndexedTokenization, ::DocumentStage, x) = enumerate(x)
function splitting(::IndexedTokenization, w::WordStage, x)
    offsets = _offsets(w)
    offsets.word += 1
    return zip(repeated(offsets), x)
end

@inline wrap(::IndexedTokenization, d::DocumentStage, (i, x)) = Sentence(x, updatemeta(getmeta(d), (sentence_id = i,)))
@inline wrap(::IndexedTokenization, s::SubSentenceStage, (i, x)) = Word(x, updatemeta(getmeta(s), (offsets = i,)))
@inline wrap(::IndexedTokenization, s::SentenceStage, (i, x)) = Word(x, updatemeta(getmeta(s), (offsets = i,)))

function wrap(::IndexedTokenization, w::WordStage, (i, x))
    meta = getmeta(w)
    if hasmeta(w) && haskey(meta, :offsets)
        offsets = meta.offsets
        word_id = offsets.word
    else
        word_id = 1
    end
    return SubWord(x, updatemeta(getmeta(w), (word_id = word_id, offsets = i,)))
end

function wrap(::IndexedTokenization, w::WordStage)
    meta = getmeta(w)
    if hasmeta(w) && haskey(meta, :offsets)
        offsets = meta.offsets
        word_id = offsets.word += 1
    else
        word_id = 1
    end
    return Token(getvalue(w), updatemeta(meta, (word_id = word_id,)))
end

function wrap(::IndexedTokenization, x::TokenStage)
    meta = getmeta(x)
    if hasmeta(x) && haskey(meta, :offsets)
        offsets = meta.offsets
        word_id = haskey(meta, :word_id) ? meta.word_id : (offsets.word += 1)
        token_id = offsets.token += 1
        meta = Base.structdiff(meta, NamedTuple{(:offsets,)})
    else
        word_id = token_id = 1
    end
    return setmeta(x, updatemeta(meta, (word_id = word_id, token_id = token_id)))
end

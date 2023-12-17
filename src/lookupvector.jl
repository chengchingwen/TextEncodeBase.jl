import DoubleArrayTries
using DoubleArrayTries: DoubleArrayTrie

lookup(dat::DoubleArrayTrie, i::Integer) = DoubleArrayTries.decode(dat, i)
lookup(dat::DoubleArrayTrie, k::Union{AbstractString, AbstractVector{UInt8}}) = DoubleArrayTries.lookup(dat, k)


abstract type LookupDict{T} <: AbstractDict{T, Int} end
LookupDict(list::AbstractVector) = LookupDict(keytype(list), list)
LookupDict(::Type{<:AbstractString}, list) = DATLookupDict(list)
LookupDict(::Type, list) = DictBackedLookupDict(list)

struct DATLookupDict <: LookupDict{String}
    trie::DoubleArrayTrie
    uid2idx::DoubleArrayTries.CVector
    idx2uid::DoubleArrayTries.CVector
end

function DATLookupDict(list::AbstractVector{<:AbstractString})
    @assert allunique(list) "All element should be unique"
    sortedlist = sort(list)
    trie = DoubleArrayTrie(sortedlist)
    uid2idx = Vector{Int}(undef, length(list))
    idx2uid = Vector{Int}(undef, length(list))
    @inbounds for (i, str) in enumerate(list)
        uid = lookup(trie, str)
        uid2idx[uid] = i
        idx2uid[i] = uid
    end
    return DATLookupDict(trie, DoubleArrayTries.CVector(uid2idx), DoubleArrayTries.CVector(idx2uid))
end

uid2idx(d::DATLookupDict, uid) = @inbounds Int(d.uid2idx[uid])
idx2uid(d::DATLookupDict, idx) = @inbounds Int(d.idx2uid[idx])

Base.length(d::DATLookupDict) = length(d.trie)
function Base.get(d::DATLookupDict, k::Union{AbstractString, AbstractVector{UInt8}}, v)
    uid = lookup(d.trie, k)
    uid == 0 && return v
    return uid2idx(d, uid)
end
function Base.iterate(d::DATLookupDict, state = nothing)
    it = iterate(d.trie, state)
    isnothing(it) && return nothing
    (key, uid), nstate = it
    val = uid2idx(d, uid)
    return key => val, nstate
end

lookup_index(d::DATLookupDict, unki, word) = get(d, word, unki)
lookup_word(d::DATLookupDict, unk, index) = 0 < index <= length(d) ? lookup(d.trie, idx2uid(d, index)) : unk

struct DictBackedLookupDict{T, D <: AbstractDict{T, Int},
                            V <: Union{AbstractVector{T}, AbstractDict{Int, T}}} <: LookupDict{T}
    dict::D
    list::V
end

function DictBackedLookupDict(list::AbstractVector)
    @assert allunique(list) "All element should be unique"
    dict = Dict{eltype(list), Int}()
    @inbounds for (i, val) in enumerate(list)
        dict[val] = i
    end
    return DictBackedLookupDict(dict, list)
end

Base.length(d::DictBackedLookupDict) = length(d.dict)
Base.get(d::DictBackedLookupDict, k, v) = get(d.dict, k, v)
Base.iterate(d::DictBackedLookupDict, state...) = iterate(d.dict, state...)

lookup_index(d::DictBackedLookupDict, unki, word) = isempty(d.dict) ? unki : get(d, word, unki)
function lookup_word(d::DictBackedLookupDict, unk, index)
    if d.list isa AbstractVector
        return 0 < index <= length(d) ? @inbounds(d.list[index]) : unk
    else
        return isempty(d.list) ? unk : get(d.list, index, unk)
    end
end

abstract type LookupVector{T} <: AbstractVector{T} end
LookupVector(list::AbstractVector) = LookupVector(eltype(list), list)
LookupVector(::Type{<:AbstractString}, list) = DATLookupVector(list)
LookupVector(::Type, list) = DictBackedLookupVector(list)

struct DATLookupVector <: LookupVector{String}
    dict::DATLookupDict
end

DATLookupVector(vector::AbstractVector) = DATLookupVector(DATLookupDict(vector))

struct DictBackedLookupVector{T, D <: LookupDict{T}} <: LookupVector{T}
    dict::D
end

DictBackedLookupVector(vector::AbstractVector) = DictBackedLookupVector(DictBackedLookupDict(vector))

Base.length(v::LookupVector) = length(v.dict)
Base.size(v::LookupVector) = (length(v),)
Base.checkbounds(::Type{Bool}, v::LookupVector, i) = 0 < i <= length(v)
@inline function Base.getindex(v::LookupVector, i::Integer)
    k = lookup_word(v, nothing, i)
    @boundscheck checkbounds(v, i)
    return k
end

lookup_index(v::LookupVector, unki, word) = lookup_index(v.dict, unki, word)
lookup_word(v::LookupVector, unk, index) = lookup_word(v.dict, unk, index)

struct OverwritableLookupVector{T, V <: LookupVector{T}, D <: DictBackedLookupDict{T}} <: LookupVector{T}
    vector::V
    dict::D
end

OverwritableLookupVector(vector::AbstractVector) = OverwritableLookupVector(LookupVector(vector))
function OverwritableLookupVector(vector::LookupVector)
    T = eltype(vector)
    dict = DictBackedLookupDict(Dict{T, Int}(), Dict{Int, T}())
    return OverwritableLookupVector(vector, dict)
end

Base.length(v::OverwritableLookupVector) = length(v.vector)
Base.size(v::OverwritableLookupVector) = (length(v),)
Base.checkbounds(::Type{Bool}, v::OverwritableLookupVector, i) = 0 < i <= length(v)
function Base.getindex(v::OverwritableLookupVector, i::Integer)
    k = lookup_word(v, nothing, i)
    @boundscheck isnothing(k)
    return k
end

function lookup_index(v::OverwritableLookupVector, unki, word)
    i = lookup_index(v.dict, 0, word)
    iszero(i) || return i
    i = lookup_index(v.vector, 0, word)
    iszero(i) && return unki
    return isnothing(lookup_word(v.dict, nothing, i)) ? i : unki
end
function lookup_word(v::OverwritableLookupVector, unk, index)
    k = lookup_word(v.dict, nothing, index)
    return isnothing(k) ? lookup_word(v.vector, unk, index) : k
end

function Base.setindex!(v::OverwritableLookupVector, val, i::Integer)
    @boundscheck checkbounds(v, i)
    @assert iszero(lookup_index(v, 0, val)) "Element must be unique, value $(repr(val)) already in the lookup vector"
    k = lookup_word(v.dict, nothing, i)
    isnothing(k) || delete!(v.dict.dict, k)
    v.dict.dict[val] = i
    v.dict.list[i] = val
    return v
end
function Base.setindex!(v::OverwritableLookupVector, val, k)
    i = lookup_index(v, 0, k)
    iszero(i) && throw(KeyError(k))
    return v[i] = val
end

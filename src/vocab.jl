using StaticArrays

abstract type AbstractVocabulary end

struct Vocab{T, A<:AbstractVector{T}} <: AbstractVocabulary
    list::A
    unk::T
    unki::Int
end

function Vocab(data::Vector, unk::AbstractString="[UNK]")
    udata = unique!(map(String, data))
    list = SizedVector{length(udata)}(udata)
    i = findfirst(==(unk), list)
    unki = isnothing(i) ? 0 : i
    return Vocab(list, String(unk), unki)
end

Base.length(v::Vocab) = length(v.list)

function Base.show(io::IO, v::Vocab)
    summary(io, v)
    print(io, "(size = ", length(v))
    print(io, ", unk = ", v.unk)
    print(io, ", unki = ", v.unki, ')')
end

lookup(v::Vocab) = Base.Fix1(lookup,  v)
lookup(v::Vocab{<:AbstractString}, s::AbstractString) = (i = findfirst(==(s), v.list); isnothing(i) ? v.unki : i)
lookup(v::Vocab{T}, s::T) where T = (i = findfirst(==(s), v.list); isnothing(i) ? v.unki : i)
lookup(v::Vocab, i::Integer) = 0 < i <= length(v.list) ? v.list[i] : v.unk
lookup(v::Vocab, i, j, k...) = (lookup(v, i), lookup(v, j), map(lookup(v), k)...)
lookup(v::Vocab, is::AbstractArray) = map(lookup(v), is)

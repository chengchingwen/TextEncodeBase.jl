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

lookup(::Type{OneHot}, v::Vocab) = lookup $ OneHot $ v
lookup(::Type{OneHot}, v::Vocab, i) = OneHot(length(v))(lookup(v, i))
lookup(T::Type{OneHot}, v::Vocab, is::AbstractArray) = OneHotArray(map(lookup(T, v), is))

function lookup(T::Type{OneHot}, v::Vocab, i, j, k...)
    c = 0
    si = lookup(T, v, i)
    c += si isa OneHot ? 1 : length(parent(si))
    sj = lookup(T, v, j)
    c += sj isa OneHot ? 1 : length(parent(sj))
    sk = map(lookup(T, v), k)
    c += sum(k->k isa OneHot ? 1 : length(parent(k)), sk; init=0)
    arr = Vector{OneHot(length(v))}(undef, c); empty!(arr)
    si isa OneHot ? push!(arr, si) : append!(arr, parent(si))
    sj isa OneHot ? push!(arr, sj) : append!(arr, parent(sj))
    for k in sk
        k isa OneHot ? push!(arr, k) : append!(arr, parent(k))
    end
    return OneHotArray(arr)
end

lookup(::Type{OneHot}, v::Vocab, i::Integer) = (s = lookup(v, i); throw(DomainError(s, "cannot convert `lookup(::Vocab, $i)` = $(repr(s)) into one-hot representation.")))

lookup(v::Vocab, i::OneHot) = lookup(v, Int(i))
lookup(v::Vocab, i::OneHotArray) = lookup(v, reinterpret(UInt32, i))

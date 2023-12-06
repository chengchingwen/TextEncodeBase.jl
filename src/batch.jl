using DataStructures: MutableLinkedList

struct Batch{S, A<:AbstractVector, M} <: TokenStages
    x::A
    meta::M
end

Batch{S}(x, meta = nothing) where S = Batch{S, typeof(x), typeof(meta)}(x, meta)

setmeta(x::Batch{S}, meta) where S = Batch{S}(x.x, meta)
setvalue(x::Batch{S}, y) where S = Batch{S}(y, x.meta)

function Base.show(io::IO, x::Batch{S}) where S
    print(io, "Batch{", S, "}(", x.x)
    isnothing(x.meta) || print(io, ", ", x.meta)
    print(io, ')')
end

splittability(::BaseTokenization, ::Batch) = Splittable()

splitting(::BaseTokenization, s::Batch) = s.x

wrap(::BaseTokenization, b::Batch{S},  x) where S = S(x, getmeta(b))

# nested

tokenize(tkr::NestedTokenizer, p::ParentStages, t::AbstractTokenization, x::Batch{Document}) = collect(tokenize_procedure!(push!, MutableLinkedList{Vector{Vector{TokenStage}}}(), tkr, p, t, x))

tokenize(tkr::NestedTokenizer, p::ParentStages, t::AbstractTokenization, x::Batch{Sentence}) = collect(tokenize_procedure!(push!, MutableLinkedList{Vector{TokenStage}}(), tkr, p, t, x))

# indexed

splitting(p::ParentStages, t::IndexedTokenization, b::Batch{Sentence}, x) = enumerate(splitting(p, t.base, b, x))

wrap(p::ParentStages, t::IndexedTokenization, b::Batch{Sentence}, (i, x)) = updatemeta(wrap(p, t.base, b, x), (sentence_id = i,))

splitting(p::ParentStages, t::IndexedTokenization, b::Batch{Document}, x) = enumerate(splitting(p, t.base, b, x))
wrap(p::ParentStages, t::IndexedTokenization, b::Batch{Document}, (i, x)) = updatemeta(wrap(p, t.base, b, x), (document_id = i,))


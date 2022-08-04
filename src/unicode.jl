using Unicode: normalize
using Base.Unicode: utf8proc_map, UTF8PROC_STABLE, UTF8PROC_COMPAT, UTF8PROC_COMPOSE,
    UTF8PROC_DECOMPOSE, UTF8PROC_IGNORE, UTF8PROC_REJECTNA, UTF8PROC_NLF2LS, UTF8PROC_NLF2PS,
    UTF8PROC_NLF2LF, UTF8PROC_STRIPCC, UTF8PROC_CASEFOLD, UTF8PROC_CHARBOUND, UTF8PROC_LUMP,
    UTF8PROC_STRIPMARK

_utf8proc_flags(nf::Symbol) = if nf === :NFC
    return UTF8PROC_STABLE | UTF8PROC_COMPOSE
elseif nf === :NFD
    return UTF8PROC_STABLE | UTF8PROC_DECOMPOSE
elseif nf === :NFKC
    return UTF8PROC_STABLE | UTF8PROC_COMPOSE | UTF8PROC_COMPAT
elseif nf === :NFKD
    return UTF8PROC_STABLE | UTF8PROC_DECOMPOSE | UTF8PROC_COMPAT
else
    throw(ArgumentError(":$nf is not one of :NFC, :NFD, :NFKC, :NFKD"))
end

function _utf8proc_flags(;
    stable::Bool=false,
    compat::Bool=false,
    compose::Bool=true,
    decompose::Bool=false,
    stripignore::Bool=false,
    rejectna::Bool=false,
    newline2ls::Bool=false,
    newline2ps::Bool=false,
    newline2lf::Bool=false,
    stripcc::Bool=false,
    casefold::Bool=false,
    lump::Bool=false,
    stripmark::Bool=false,
                        )
    flags = 0
    stable && (flags = flags | UTF8PROC_STABLE)
    compat && (flags = flags | UTF8PROC_COMPAT)
    if decompose
        flags = flags | UTF8PROC_DECOMPOSE
    elseif compose
        flags = flags | UTF8PROC_COMPOSE
    elseif compat || stripmark
        throw(ArgumentError("compat=true or stripmark=true require compose=true or decompose=true"))
    end
    stripignore && (flags = flags | UTF8PROC_IGNORE)
    rejectna && (flags = flags | UTF8PROC_REJECTNA)
    newline2ls + newline2ps + newline2lf > 1 && throw(ArgumentError("only one newline conversion may be specified"))
    newline2ls && (flags = flags | UTF8PROC_NLF2LS)
    newline2ps && (flags = flags | UTF8PROC_NLF2PS)
    newline2lf && (flags = flags | UTF8PROC_NLF2LF)
    stripcc && (flags = flags | UTF8PROC_STRIPCC)
    casefold && (flags = flags | UTF8PROC_CASEFOLD)
    lump && (flags = flags | UTF8PROC_LUMP)
    stripmark && (flags = flags | UTF8PROC_STRIPMARK)
    return flags
end

struct UnicodeNormalizer{T} <: SentenceNormalizer{T}
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
        flags = t.flags
        (flags & UTF8PROC_STABLE > 0) &&
            print(io, ", stable = true")
        (flags & UTF8PROC_COMPAT > 0) &&
            print(io, ", compat = true")
        (flags & UTF8PROC_DECOMPOSE > 0) &&
            print(io, ", decompose = true")
        (flags & UTF8PROC_COMPOSE > 0) &&
            print(io, ", compose = true")
        (flags & UTF8PROC_IGNORE > 0) &&
            print(io, ", stripignore = true")
        (flags & UTF8PROC_REJECTNA > 0) &&
            print(io, ", rejectna = true")
        (flags & UTF8PROC_NLF2LS > 0) &&
            print(io, ", newline2ls = true")
        (flags & UTF8PROC_NLF2PS > 0) &&
            print(io, ", newline2ps = true")
        (flags & UTF8PROC_NLF2LF > 0) &&
            print(io, ", newline2lf = true")
        (flags & UTF8PROC_STRIPCC > 0) &&
            print(io, ", stripcc = true")
        (flags & UTF8PROC_CASEFOLD > 0) &&
            print(io, ", casefold = true")
        (flags & UTF8PROC_LUMP > 0) &&
            print(io, ", lump = true")
        (flags & UTF8PROC_STRIPMARK > 0) &&
            print(io, ", stripmark = true")
        print(io, ')')
    else
        name = nfs[idx]
        print(io, name, '(')
        show(io, base(t))
        print(io, ')')
    end
end

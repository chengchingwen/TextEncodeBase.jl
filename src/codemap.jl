_larger_type(a, b) = sizeof(a) >= sizeof(b) ? a : b

function codesize(crs)
    T = UInt8
    for cr in crs
        x = UInt32(last(cr))
        xT = if x > UInt(typemax(UInt16))
            UInt32
        elseif x > UInt(typemax(UInt8))
            UInt16
        else
            UInt8
        end
        T = _larger_type(T, xT)
    end
    return T
end

function code_range(f, t)
    fr = code_range(f)
    tr = code_range(t)
    @assert length(fr) == length(tr) "codemap of two range with different length: $(length(fr)) != $(length(tr))"
    return (fr, tr)
end
code_range(arg::NTuple{2}) = code_range(arg...)
code_range(arg::Pair) = code_range(arg[1], arg[2])
code_range(c::Integer) = code_range(Char(c))
code_range(c::Char) = c:c
code_range(r::UnitRange) = Char(r.start):Char(r.stop)
code_range(r::StepRange) = StepRange(Char(r.start), Int(r.step), Char(r.stop))
code_range(r::StepRange{Char}) = StepRange(r.start, Int(r.step), r.stop)
code_range(r::StepRange{Char, Int}) = r

struct CodeMap{F, T}
    from::Vector{StepRange{Char, Int}}
    to::Vector{StepRange{Char, Int}}
    function CodeMap(
        from::Vector{StepRange{Char, Int}},
        to::Vector{StepRange{Char, Int}},
    )
        From = codesize(from)::Type{<:Union{UInt8, UInt16, UInt32}}
        To = codesize(to)::Type{<:Union{UInt8, UInt16, UInt32}}
        @assert length(from) == length(to) "different number of code ranges: $(length(from)) != $(length(to))"
        return new{From, To}(sort(from), sort(to))
    end
end
(cm::CodeMap)(x) = codemap(cm, x)

CodeMap(args...) = CodeMap(args)
function CodeMap(args)
    len = length(args)
    from = Vector{StepRange{Char, Int}}(undef, len)
    to = Vector{StepRange{Char, Int}}(undef, len)
    for (i, arg) in enumerate(args)
        from[i], to[i] = code_range(arg)
    end
    return CodeMap(from, to)
end

struct CodeUnMap{F, T}
    codemap::CodeMap{F, T}
end
(um::CodeUnMap)(x) = codeunmap(um.codemap, x)


function find_code(rs, c)
    @inbounds for (i, r) in enumerate(rs)
        j = findfirst(==(c), r)
        isnothing(j) && continue
        return (i, j)
    end
    return nothing
end

function codemap(cm::CodeMap{F, T}, c::Char) where {F, T}
    I = find_code(cm.from, c)
    x = isnothing(I) ? c : cm.to[I[1]][I[2]]
    return T(x)
end
codemap(cm::CodeMap, x::Integer) = codemap(cm, Char(x))
codemap(cm::CodeMap{F}, x::AbstractString) where F =
    transcode(String, map(Base.Fix1(codemap, cm), transcode(F, codeunits(x))))

function codeunmap(cm::CodeMap{F, T}, c::Char) where {F, T}
    I = find_code(cm.to, c)
    x = isnothing(I) ? c : cm.from[I[1]][I[2]]
    return F(x)
end
codeunmap(cm::CodeMap, x::Integer) = codeunmap(cm, Char(x))
codeunmap(cm::CodeMap{F, T}, x::AbstractString) where {F, T} =
    transcode(String, map(Base.Fix1(codeunmap, cm), transcode(T, codeunits(x))))


function Base.show(io::IO, cm::CodeMap{F,T}) where {F, T}
    print(io, "CodeMap{", F, " => ", T, '}')
    print(io, '(', length(cm.to), " code-ranges)")
end

function Base.show(io::IO, um::CodeUnMap{F, T}) where {F, T}
    print(io, "CodeUnMap{", F, " <= ", T, '}')
    print(io, '(', length(um.codemap.to), " code-ranges)")
end

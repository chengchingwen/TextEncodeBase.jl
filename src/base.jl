using WordTokenizers: rulebased_split_sentences, nltk_word_tokenize

abstract type TokenStages end
abstract type DocumentStage    <: TokenStages end
abstract type SentenceStage    <: TokenStages end
abstract type SubSentenceStage <: TokenStages end
abstract type WordStage        <: TokenStages end
abstract type TokenStage       <: TokenStages end

struct Document{T, M}    <: DocumentStage    ; x::T; meta::M; end
struct Sentence{T, M}    <: SentenceStage    ; x::T; meta::M; end
struct SubSentence{T, M} <: SubSentenceStage ; x::T; meta::M; end
struct Word{T, M}        <: WordStage        ; x::T; meta::M; end
struct Token{T, M}       <: TokenStage       ; x::T; meta::M; end

Document(x) = Document(x, nothing)
Sentence(x) = Sentence(x, nothing)
SubSentence(x) = SubSentence(x, nothing)
Word(x) = Word(x, nothing)
Token(x) = Token(x, nothing)

Base.show(io::IO, t::TokenStages) = 
    print(io, typeof(t).name.name, ntuple(i->getfield(t, i), fieldcount(typeof(t))))

@inline splitting(::AbstractTokenizer, d::DocumentStage) = rulebased_split_sentences(d.x)
@inline splitting(::AbstractTokenizer, s::SentenceStage) = nltk_word_tokenize(s.x)
@inline splitting(::AbstractTokenizer, s::SubSentenceStage) = nltk_word_tokenize(s.x)

@inline tokenize(t::AbstractTokenizer, ::DocumentStage, x) = tokenize(t, Sentence(x))
@inline tokenize(t::AbstractTokenizer, ::SentenceStage, x) = tokenize(t, Token(x))

@inline tokenize(t::AbstractTokenizer, ::TokenStages, x::TokenStages) = tokenize(t, x)
@inline tokenize(::AbstractTokenizer, ::TokenStages, t::TokenStage) = t

@inline tokenize(::AbstractTokenizer, w::WordStage) = Token(w.x)
@inline tokenize(::AbstractTokenizer, t::TokenStage) = t
function tokenize(t::AT, x::TS) where {AT <: AbstractTokenizer, TS <: TokenStages}
    v = TokenStages[]
    isempty(x.x) && return v
    for s in splitting(t, x)
        v1 = tokenize(t, x, s)
        if v1 isa AbstractVector
            append!(v, v1)
        else
            push!(v, v1)
        end
    end
    return v
end

(t::AbstractTokenizer)(x::TS) where {TS <: TokenStages} = tokenize(t, x)

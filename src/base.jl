using WordTokenizers: rulebased_split_sentences, nltk_word_tokenize

"""
abstract type for type that wrap input into specific stage for control tokenization.

There are six builtin stages in TextEncodeBase (all abstract XStage <: TokenStages):

    1. Document <: DocumentStage: the input string is a full document,
     and thus need to be splitted into multiple sentence.
    2. Sentence <: SentenceStage: the input string is a full string,
     and thus need to be splitted into multiple part (SubSentence/Word/Token).
    3. SubSentence <: SubSentenceStage: special wrapper for case where the tokenizer
     does not directly break sentence all into words/tokens and these pieces contain
     multiple words/tokens, but you need the information that they are not full sentence.
    4. Word <: WordStage: the input string is a single word.
    5. SubWord <: SubWordStage: similar to SubSentence, but for word.
    6. Token <: TokenStage: the final piece of the tokenization process.
     Generally, it's used to specify the end of this piece and should
     never be splitted.

Each wrapper have two field: `x` for the input, `meta` for extra information (`nothing` if not provided).
"""
abstract type TokenStages end
abstract type DocumentStage    <: TokenStages end
abstract type SentenceStage    <: TokenStages end
abstract type SubSentenceStage <: TokenStages end
abstract type WordStage        <: TokenStages end
abstract type SubWordStage     <: TokenStages end
abstract type TokenStage       <: TokenStages end

struct Document{T, M}    <: DocumentStage    ; x::T; meta::M; end
struct Sentence{T, M}    <: SentenceStage    ; x::T; meta::M; end
struct SubSentence{T, M} <: SubSentenceStage ; x::T; meta::M; end
struct Word{T, M}        <: WordStage        ; x::T; meta::M; end
struct SubWord{T, M}     <: SubWordStage     ; x::T; meta::M; end
struct Token{T, M}       <: TokenStage       ; x::T; meta::M; end

Document(x) = Document(x, nothing)
Sentence(x) = Sentence(x, nothing)
SubSentence(x) = SubSentence(x, nothing)
Word(x) = Word(x, nothing)
Token(x) = Token(x, nothing)

function Base.show(io::IO, t::TokenStages)
    print(io, typeof(t).name.name)
    vs = filter(!isnothing, ntuple(i->getfield(t, i), fieldcount(typeof(t))))
    if length(vs) == 1
        print(io, '(')
        show(io, vs[1])
        print(io, ')')
    else
        print(io, vs)
    end
end

@inline splitting(::AbstractTokenizer, ::TokenStages, x) = x
@inline splitting(t::AbstractTokenizer, d::DocumentStage) = splitting(t, d, rulebased_split_sentences(d.x))
@inline splitting(t::AbstractTokenizer, s::SentenceStage) = splitting(t, s, nltk_word_tokenize(s.x))
@inline splitting(t::AbstractTokenizer, s::SubSentenceStage) = splitting(t, s, nltk_word_tokenize(s.x))

@inline tokenize(t::AbstractTokenizer, ::DocumentStage, x) = tokenize(t, Sentence(x))
@inline tokenize(t::AbstractTokenizer, ::SentenceStage, x) = tokenize(t, Token(x))

@inline tokenize(t::AbstractTokenizer, ::TokenStages, x::TokenStages) = tokenize(t, x)
@inline tokenize(::AbstractTokenizer, ::TokenStages, t::TokenStage) = t

@inline tokenize(::AbstractTokenizer, w::WordStage) = Token(w.x)
@inline tokenize(::AbstractTokenizer, w::SubWordStage) = Token(w.x)
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

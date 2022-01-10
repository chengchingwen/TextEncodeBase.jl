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


let ATR = AbstractTokenizer, AT = AbstractTokenization
    # [full dispatch, default to ignore tokenizer]
    global @inline splitting(::ATR, t::AT, x::TokenStages)    = splitting(t, x)
    global @inline splitting(::ATR, t::AT, s::TokenStages, x) = splitting(t, s, x)
    # [tokenization dispatch] default splitting callback
    global @inline splitting(::AT, ::TokenStages, x) = x
    # [tokenization dispatch] default splitting behavior on specific stages
    global @inline splitting(t::AT, d::DocumentStage)    = rulebased_split_sentences(d.x)
    global @inline splitting(t::AT, s::SentenceStage)    = nltk_word_tokenize(s.x)
    global @inline splitting(t::AT, s::SubSentenceStage) = nltk_word_tokenize(s.x)

    # [full dispatch, default to ignore tokenizer]
    global @inline tokenize(tkr::ATR, t::AT, s::TokenStages, x) = tokenize(t, s, x)
    # [tokenization dispatch] default behavior on specific stages, mark the splitting result for further tokenization
    global @inline tokenize(::AT, ::DocumentStage, x) = Sentence(x)
    global @inline tokenize(::AT, ::SentenceStage, x) = Token(x)
    # [tokenization dispatch] skip if splitting result is already wrapped
    global @inline tokenize(::AT, ::TokenStages, x::TokenStages) = x
    # [full dispatch, default to ignore tokenizer] the outer-most api, but these stages are usually unsplittable
    global @inline tokenize(::ATR, t::AT, x::Union{WordStage, SubWordStage, TokenStage}) = tokenize(t, x)
    # [tokenization dispatch] default tokenization for these stages
    global @inline tokenize(::AT, w::WordStage)    = [Token(w.x)]
    global @inline tokenize(::AT, w::SubWordStage) = [Token(w.x)]
    global @inline tokenize(::AT, t::TokenStage)   = [t]
end

# [full dispatch] the outer-most api, splitting input and recursively tokenize the result. ignore if input is empty
function tokenize(tkr::ATR, t::AT, x::TS) where {ATR <: AbstractTokenizer, AT <: AbstractTokenization, TS <: TokenStages}
    v = TokenStage[]
    isempty(x.x) && return v
    for sp in splitting(tkr, t, x, splitting(tkr, t, x))
        v1 = tokenize(tkr, t, tokenize(tkr, t, x, sp))
        append!(v, v1)
    end
    return v
end

"""
    splitting(t::AbstractTokenization, x::TokenStages)

Split `x` given its tokenization stage. For example,
 the default behavior of a document stage is splitting into
 sentences (with `WordTokenizers.split_sentences`).

Overload this method for custom tokenization.
"""
function splitting end

@eval $((@macroexpand @doc """
    splitting(t::AbstractTokenization, s::TokenStages, x)

Interface for providing callback for splitting. `x` is the result of `splitting(t, s)`.

Overload this method for custom `splitting` callback.
"""
function splitting(::AbstractTokenization, ::TokenStages, x) end
).args[2])

"""
    tokenize(t::AbstractTokenization, s::TokenStages, x)

Mark the tokenization stage of `x`, which is part of the splitting result of `s`.
 For example, if we are doing simple whitespace tokenization and at the sentence stage,
 then `x` is just single word of `s` and thus return `Word(x)` (or `Token(x)`).
 Skip if `x` is already a `TokenStages`.

Overload this method to control the tokenization process.
"""
function tokenize end

@eval $((@macroexpand @doc """
    tokenize(t::AbstractTokenization, x::TokenStages)

Tokenize `x`.

Overload this method for custom tokenization and stages.
"""
function tokenize(t::AbstractTokenization, x::TokenStages)end
).args[2])

# tokenizer api
(t::AbstractTokenizer)(x::TS) where {TS <: TokenStages} = tokenize(t, tokenization(t), x)

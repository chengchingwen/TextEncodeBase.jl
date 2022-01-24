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
SubWord(x) = SubWord(x, nothing)
Token(x) = Token(x, nothing)

getvalue(x::TokenStages) = x.x
getmeta(x::TokenStages) = x.meta
hasmeta(x::TokenStages) = !isnothing(getmeta(x))

setmeta(x::Document, meta) = Document(x.x, meta)
setmeta(x::Sentence, meta) = Sentence(x.x, meta)
setmeta(x::SubSentence, meta) = SubSentence(x.x, meta)
setmeta(x::Word, meta) = Word(x.x, meta)
setmeta(x::SubWord, meta) = SubWord(x.x, meta)
setmeta(x::Token, meta) = Token(x.x, meta)

updatemeta(::Nothing, meta) = meta
updatemeta(a::NamedTuple, meta::NamedTuple) = merge(a, meta)

updatemeta(x::TokenStages, meta) = setmeta(x, updatemeta(getmeta(x), meta))

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
    global @inline splitting(t::AT, d::DocumentStage)    = rulebased_split_sentences(getvalue(d))
    global @inline splitting(t::AT, s::SentenceStage)    = nltk_word_tokenize(getvalue(s))
    global @inline splitting(t::AT, s::SubSentenceStage) = nltk_word_tokenize(getvalue(s))

    # [full dispatch, default to ignore tokenizer] splittable (4-arg) & unsplittable (3-arg)
    global @inline wrap(tkr::ATR, t::AT, s::TokenStages, x) = wrap(t, s, x)
    global @inline wrap(tkr::ATR, t::AT, s::TokenStages)    = wrap(t, s)
    # [tokenization dispatch] default behavior on specific stages, mark the splitting result for further tokenization
    global @inline wrap(::AT, d::DocumentStage, x) = Sentence(x, getmeta(d))
    global @inline wrap(::AT, s::SentenceStage, x) = Word(x, getmeta(s))
    global @inline wrap(::AT, s::SubSentenceStage, x) = Word(x, getmeta(s))
    # [tokenization dispatch] default skip if splitting result is already wrapped
    global @inline wrap(::AT, ::TokenStages, x::TokenStages) = x
    # [tokenization dispatch] default mark unsplittable as token
    global @inline wrap(::AT, w::WordStage)    = Token(getvalue(w), getmeta(w))
    global @inline wrap(::AT, w::SubWordStage) = Token(getvalue(w), getmeta(w))
    global @inline wrap(::AT, t::TokenStage)   = t

    # the outer-most api, splitting input and recursively tokenize the result. ignore if input is empty
    global @inline tokenize(tkr::ATR, t::AT, x::TokenStages) = tokenize_procedure(tkr, t, x)
    global @inline tokenize(tkr::ATR, t::AT, s::Union{WordStage, SubWordStage}) = tokenize(tkr, t, s, wrap(tkr, t, s))
    global @inline tokenize(tkr::ATR, t::AT, x::TokenStage) = [wrap(tkr, t, x)]
    # 4-arg tokenize for distinguishing recursive call and direct call
    global @inline tokenize(tkr::ATR, t::AT, ::TokenStages, x::TokenStages) = tokenize(tkr, t, x)
    global @inline tokenize(tkr::ATR, t::AT, ::Nothing, x::TokenStages)     = tokenize(tkr, t, x)
end

"""
    tokenization_procedure(tokenizer, tokenizaton, stage)

The procedure of tokenization (`splitting` + `wrap` + `tokenize`).
 This is use to restore full behavior for stage that default
 unsplittable. Generally don't overload this function.
"""
@inline tokenize_procedure(tkr, t, x) = tokenize_procedure!(append!, TokenStage[], tkr, t, x)

function tokenize_procedure!(op, v, tkr, t, x)
    isempty(getvalue(x)) && return v
    for sp in splitting(tkr, t, x, splitting(tkr, t, x))
        v1 = tokenize(tkr, t, x, wrap(tkr, t, x, sp))
        op(v, v1)
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
    wrap(t::AbstractTokenization, s::TokenStages, x)

Mark the tokenization stage of `x`, which is part of the splitting result of `s`.
 For example, if we are doing simple whitespace tokenization and at the sentence stage,
 then `x` is just single word of `s` and thus return `Word(x)` (or `Token(x)`).
 Skip if `x` is already a `TokenStages`. (this method only apply to splittable stages)

Overload this method to control the tokenization process.
"""
function wrap end

@eval $((@macroexpand @doc """
    wrap(t::AbstractTokenization, x::TokenStages)

A handler for unsplittable stages (token/word/...).

Overload this method for custom transform.
"""
function wrap(t::AbstractTokenization, x::TokenStages) end
).args[2])

@eval $((@macroexpand @doc """
    tokenize(tkr::AbstractTokenizer, t::AbstractTokenization, x::TokenStages)

Tokenize `x` according to `tkr` and `t`.

Overload for custom tokenizer, tokenization and stages. For making a unsplittable
 into splittable (or vice versa), you must overload this method.
"""
function tokenize(tkr::AbstractTokenizer, t::AbstractTokenization, x::TokenStages) end
).args[2])


# tokenizer api
(t::AbstractTokenizer)(x::TS) where {TS <: TokenStages} = tokenize(t, tokenization(t), nothing, x)

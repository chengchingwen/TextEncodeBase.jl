using WordTokenizers: rulebased_split_sentences, nltk_word_tokenize

"""
abstract type for tokenizers.

Each tokenizer is link with a tokenization (by
 defining `tokenization(::Tokenizer) = Tokenization()`).
 The overall framework dispatch on both tokenizer and
 tokenization, but most of the time we only add methods
 for tokenization. This allow further composability and
 can interfere the tokenization process with given
 tokenizer.
"""
abstract type AbstractTokenizer end

"""
abstract type for tokenization.

The tokenization procedure is separate into multiple
 `TokenStages` and recursive calls of `splitting`, `wrap`,
 and `tokenize`. `splitting` break string into substrings,
 `wrap` mark the substrings with new `TokenStages`, and
 `tokenize` is responsible for the tokenization.
"""
abstract type AbstractTokenization end

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

setvalue(x::Document, y) = Document(y, x.meta)
setvalue(x::Sentence, y) = Sentence(y, x.meta)
setvalue(x::SubSentence, y) = SubSentence(y, x.meta)
setvalue(x::Word, y) = Word(y, x.meta)
setvalue(x::SubWord, y) = SubWord(y, x.meta)
setvalue(x::Token, y) = Token(y, x.meta)

updatemeta(::Nothing, meta) = meta
updatemeta(a::NamedTuple, meta::NamedTuple) = merge(a, meta)

updatevalue(f, x::TokenStages) = setvalue(x, f(getvalue(x)))
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

const ParentStages = Union{Nothing, TokenStages}

"""
splittability trait

The splittability trait decide whether the given combination (tokenizer x tokenization x stage) is
 splittable or not (`Splittable` or `UnSplittable`). For example, `DefaultTokenization` and `SentenceStage`
 is splittable (i.e. `splittability(::DefaultTokenization, ::SentenceStage) = Splittable()`). The splittability
 change the behavior of `tokenize`: if it's splittable, `tokenize` will try to call `splitting` on the input,
 `wrap` each splitting result and recurse. Otherwise, it will directly call `wrap` and then recurse into `tokenize`.
"""
abstract type Splittability end
struct Splittable <: Splittability end
struct UnSplittable <: Splittability end

"""
    splittability(args...)

Return the splittability (`Splittable`/`UnSplittable`) of given argument combination.
 Overload to make a `TokenStages` splittable.
"""
function splittability end

"""
    splittable(args...)

Return `true` if the splittability of given argument combination is `Splittable()`.
"""
splittable(args...) = splittable(splittability(args...))
splittable(::Splittable)   = true
splittable(::UnSplittable) = false

splitting(::typeof(splittability), args...) = splitting(splittability(args...), args...)
splitting(::Splittable, args...) = splitting(args...)
splitting(::UnSplittable, args...) = error("Argument is unsplittable: ", args)

# dispatch: tokenizer -> parent stage -> tokenization -> token stage
let ATR = AbstractTokenizer, AT = AbstractTokenization
    # splittability: overload to make specific combination splittable
    global @inline splittability(tkr::ATR, t::AT, x::TokenStages) = splittability(tkr, nothing, t, x)
    global @inline splittability(tkr::ATR, s::ParentStages, t::AT, x::TokenStages) = splittability(s, t, x)
    global @inline splittability(::ParentStages, t::AT, x::TokenStages) = splittability(t, x)
    global @inline splittability(t::AT, x::TokenStages) = UnSplittable()
    # after `Splittable`, define how to split it
    global @inline splitting(::ATR, p::ParentStages, t::AT, x::TokenStages) = splitting(p, t, x)
    global @inline splitting(p::ParentStages, t::AT, x::TokenStages) = splitting(t, x)
    # a callback for `splitting`, `x` is the result of `splitting(::ATR, ::ParentStages,  ::TokenStages)`
    global @inline splitting(::ATR, p::ParentStages, t::AT, s::TokenStages, x) = splitting(p, t, s, x)
    global @inline splitting(p::ParentStages, t::AT, s::TokenStages, x) = splitting(t, s, x)
    global @inline splitting(::AT, ::TokenStages, x) = x
    # splittable (4-arg): wrap the splitting result into specific `TokenStages`, e.g. "word" => Word("word")
    global @inline wrap(::ATR, p::ParentStages, t::AT, s::TokenStages, x) = wrap(p, t, s, x)
    global @inline wrap(p::ParentStages, t::AT, s::TokenStages, x) = wrap(t, s, x)
    global @inline wrap(::AT, ::TokenStages, x::TokenStages) = x # already wrapped
    # unsplittable (3-arg): transform the input into next `TokenStages`, e.g. Word("word") => Token("word")
    global @inline wrap(::ATR, p::ParentStages, t::AT, x::TokenStages)    = wrap(p, t, x)
    global @inline wrap(p::ParentStages, t::AT, s::TokenStages)    = wrap(t, s)
    # the outer-most api, splitting input and recursively tokenize the result. ignore if input is empty
    global @inline tokenize(tkr::ATR, t::AT, x::TokenStages) = tokenize(tkr, nothing, t, x)
    global @inline tokenize(tkr::ATR, s::ParentStages, t::AT, x::TokenStages) = tokenize_procedure(tkr, s, t, x)
    global @inline tokenize(tkr::ATR, s::ParentStages, t::AT, x::TokenStage) = isempty(getvalue(x)) ? TokenStage[] : TokenStage[wrap(tkr, s, t, x)]
end

"""
    tokenization_procedure(tokenizer, tokenizaton, stage)

The procedure of tokenization (`splitting` + `wrap` + `tokenize`).
"""
@inline tokenize_procedure(tkr, t, x) = tokenize_procedure(tkr, t, nothing, x)
@inline tokenize_procedure(tkr, s, t, x) = tokenize_procedure!(append!, splittability, TokenStage[], tkr, s, t, x)
@inline tokenize_procedure!(op, v, tkr, s, t, x) = tokenize_procedure!(op, splittability, v, tkr, s, t, x)
@inline tokenize_procedure!(op, ::typeof(splittability), v, tkr, s, t, x) = tokenize_procedure!(op, splittability(tkr, s, t, x), v, tkr, s, t, x)

function tokenize_procedure!(op, ::Splittable, v, tkr, s, t, x)
    isempty(getvalue(x)) && return v
    for sp in splitting(tkr, s, t, x, splitting(splittability, tkr, s, t, x))
        v1 = tokenize(tkr, x, t, wrap(tkr, s, t, x, sp))
        op(v, v1)
    end
    return v
end

function tokenize_procedure!(op, ::UnSplittable, v, tkr, s, t, x)
    isempty(getvalue(x)) && return v
    op(v, tokenize(tkr, x, t, wrap(tkr, s, t, x)))
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


# abstract type for convenience

abstract type BaseTokenization <: AbstractTokenization end

struct DefaultTokenization <: BaseTokenization end

splittability(::BaseTokenization, x::Union{DocumentStage, SentenceStage, SubSentenceStage}) = Splittable()
splittability(::BaseTokenization, x::Union{WordStage, SubWordStage}) = UnSplittable()

splitting(::BaseTokenization, d::DocumentStage)    = rulebased_split_sentences(getvalue(d))
splitting(::BaseTokenization, s::SentenceStage)    = nltk_word_tokenize(getvalue(s))
splitting(::BaseTokenization, s::SubSentenceStage) = nltk_word_tokenize(getvalue(s))

wrap(::BaseTokenization, d::DocumentStage, x) = Sentence(x, getmeta(d))
wrap(::BaseTokenization, s::SentenceStage, x) = Word(x, getmeta(s))
wrap(::BaseTokenization, s::SubSentenceStage, x) = Word(x, getmeta(s))
wrap(::BaseTokenization, w::WordStage, x) = SubWord(x, getmeta(w))

wrap(::BaseTokenization, w::WordStage)    = Token(getvalue(w), getmeta(w))
wrap(::BaseTokenization, w::SubWordStage) = Token(getvalue(w), getmeta(w))
wrap(::BaseTokenization, t::TokenStage)   = t

abstract type WrappedTokenization{T<:AbstractTokenization} <: AbstractTokenization end

base(t::WrappedTokenization) = t.base

splittability(p::Union{Nothing, TokenStages}, t::WrappedTokenization, x::TokenStages) = splittability(p, base(t), x)
splitting(p::Union{Nothing, TokenStages}, t::WrappedTokenization, x::TokenStages)    = splitting(p, base(t), x)
splitting(p::Union{Nothing, TokenStages}, t::WrappedTokenization, s::TokenStages, x) = splitting(p, base(t), s, x)

wrap(p::Union{Nothing, TokenStages}, t::WrappedTokenization, s::TokenStages, x) = wrap(p, base(t), s, x)
wrap(p::Union{Nothing, TokenStages}, t::WrappedTokenization, s::TokenStages)    = wrap(p, base(t), s)


# tokenizer api

"""
    tokenization(::AbstractTokenizer) :: AbstractTokenization

Return the tokenization type of given tokenizer.
"""
tokenization(::AbstractTokenizer) = DefaultTokenization()

"""
    preprocess(tkr::AbstractTokenizer, x)

Preprocess the input `x`. This is only called during `tkr(x)`.
"""
preprocess(t::AbstractTokenizer, x::TokenStages) = updatevalue(Base.Fix1(preprocess, t), x)
preprocess(t::AbstractTokenizer, x) = x

(t::AbstractTokenizer)(x::TS) where {TS <: TokenStages} = tokenize(t, nothing, tokenization(t), preprocess(t, x))

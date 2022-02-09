abstract type Tokenization <: AbstractTokenization end

struct DefaultTokenization <: Tokenization end

splittability(::Tokenization, x::Union{DocumentStage, SentenceStage, SubSentenceStage}) = Splittable()
splittability(::Tokenization, x::Union{WordStage, SubWordStage}) = UnSplittable()

splitting(::Tokenization, d::DocumentStage)    = rulebased_split_sentences(getvalue(d))
splitting(::Tokenization, s::SentenceStage)    = nltk_word_tokenize(getvalue(s))
splitting(::Tokenization, s::SubSentenceStage) = nltk_word_tokenize(getvalue(s))

# [tokenization dispatch] default behavior on specific stages, mark the splitting result for further tokenization
wrap(::Tokenization, d::DocumentStage, x) = Sentence(x, getmeta(d))
wrap(::Tokenization, s::SentenceStage, x) = Word(x, getmeta(s))
wrap(::Tokenization, s::SubSentenceStage, x) = Word(x, getmeta(s))
wrap(::Tokenization, w::WordStage, x) = SubWord(x, getmeta(w))

# [tokenization dispatch] default mark unsplittable as token
wrap(::Tokenization, w::WordStage)    = Token(getvalue(w), getmeta(w))
wrap(::Tokenization, w::SubWordStage) = Token(getvalue(w), getmeta(w))
wrap(::Tokenization, t::TokenStage)   = t

abstract type WrappedTokenization{T<:AbstractTokenization} <: AbstractTokenization end

base(t::WrappedTokenization) = t.base

let WT=WrappedTokenization
    global @inline splittability(p::Union{Nothing, TokenStages}, t::WT, x::TokenStages) = splittability(p, base(t), x)
    global @inline splitting(p::Union{Nothing, TokenStages}, t::WT, x::TokenStages)    = splitting(p, base(t), x)
    global @inline splitting(p::Union{Nothing, TokenStages}, t::WT, s::TokenStages, x) = splitting(p, base(t), s, x)
    global @inline wrap(p::Union{Nothing, TokenStages}, t::WT, s::TokenStages, x) = wrap(p, base(t), s, x)
    global @inline wrap(p::Union{Nothing, TokenStages}, t::WT, s::TokenStages)    = wrap(p, base(t), s)
end

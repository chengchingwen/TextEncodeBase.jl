abstract type BaseTokenization <: AbstractTokenization end

struct DefaultTokenization <: BaseTokenization end

splittability(::BaseTokenization, x::Union{DocumentStage, SentenceStage, SubSentenceStage}) = Splittable()
splittability(::BaseTokenization, x::Union{WordStage, SubWordStage}) = UnSplittable()

splitting(::BaseTokenization, d::DocumentStage)    = rulebased_split_sentences(getvalue(d))
splitting(::BaseTokenization, s::SentenceStage)    = nltk_word_tokenize(getvalue(s))
splitting(::BaseTokenization, s::SubSentenceStage) = nltk_word_tokenize(getvalue(s))

# [tokenization dispatch] default behavior on specific stages, mark the splitting result for further tokenization
wrap(::BaseTokenization, d::DocumentStage, x) = Sentence(x, getmeta(d))
wrap(::BaseTokenization, s::SentenceStage, x) = Word(x, getmeta(s))
wrap(::BaseTokenization, s::SubSentenceStage, x) = Word(x, getmeta(s))
wrap(::BaseTokenization, w::WordStage, x) = SubWord(x, getmeta(w))

# [tokenization dispatch] default mark unsplittable as token
wrap(::BaseTokenization, w::WordStage)    = Token(getvalue(w), getmeta(w))
wrap(::BaseTokenization, w::SubWordStage) = Token(getvalue(w), getmeta(w))
wrap(::BaseTokenization, t::TokenStage)   = t

abstract type WrappedTokenization{T<:AbstractTokenization} <: AbstractTokenization end

base(t::WrappedTokenization) = t.base

let WT=WrappedTokenization
    global @inline splittability(p::Union{Nothing, TokenStages}, t::WT, x::TokenStages) = splittability(p, base(t), x)
    global @inline splitting(p::Union{Nothing, TokenStages}, t::WT, x::TokenStages)    = splitting(p, base(t), x)
    global @inline splitting(p::Union{Nothing, TokenStages}, t::WT, s::TokenStages, x) = splitting(p, base(t), s, x)
    global @inline wrap(p::Union{Nothing, TokenStages}, t::WT, s::TokenStages, x) = wrap(p, base(t), s, x)
    global @inline wrap(p::Union{Nothing, TokenStages}, t::WT, s::TokenStages)    = wrap(p, base(t), s)
end

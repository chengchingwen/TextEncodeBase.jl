struct IndexedTokenization <: AbstractTokenization end

@inline splitting(::IndexedTokenization, ::TokenStages, x) = enumerate(x)

@inline tokenize(t::IndexedTokenization, ::DocumentStage, (i, x)) = Sentence(x, (sentence_id = i,))
@inline tokenize(t::IndexedTokenization, s::SentenceStage, (i, x)) = Token(x, merge(s.meta, (token_id = i,)))

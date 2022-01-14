struct IndexedTokenization <: AbstractTokenization end

@inline splitting(::IndexedTokenization, ::TokenStages, x) = enumerate(x)

@inline tokenize(t::IndexedTokenization, d::DocumentStage, (i, x)) = Sentence(x, updatemeta(d.meta, (sentence_id = i,)))
@inline tokenize(t::IndexedTokenization, s::SentenceStage, (i, x)) = Word(x, updatemeta(s.meta, (token_id = i,)))

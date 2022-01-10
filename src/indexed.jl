abstract type IndexedTokenizer <: AbstractTokenizer end

@inline splitting(::IndexedTokenizer, ::TokenStages, x) = enumerate(x)

@inline tokenize(t::IndexedTokenizer, ::DocumentStage, (i, x)) = tokenize(t, Sentence(x, (sentence_id = i,)))

@inline tokenize(t::IndexedTokenizer, s::SentenceStage, (i, x)) = tokenize(t, Token(x, merge(s.meta, (token_id = i,))))

struct NaiveIndexedTokenizer <: IndexedTokenizer end

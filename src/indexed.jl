abstract type IndexedTokenizer <: AbstractTokenizer end

@inline splitting(::IndexedTokenizer, d::DocumentStage) = rulebased_split_sentences(d.x) |> enumerate
@inline splitting(::IndexedTokenizer, s::SentenceStage) = nltk_word_tokenize(s.x) |> enumerate
@inline splitting(::IndexedTokenizer, s::SubSentenceStage) = nltk_word_tokenize(s.x) |> enumerate

@inline tokenize(t::IndexedTokenizer, ::DocumentStage, (i, x)) = tokenize(t, Sentence(x, (sentence_id = i,)))

@inline tokenize(t::IndexedTokenizer, s::SentenceStage, (i, x)) = tokenize(t, Token(x, merge(s.meta, (token_id = i,))))

struct NaiveIndexedTokenizer <: IndexedTokenizer end

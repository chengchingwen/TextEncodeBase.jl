"tokenizer that run the default behavior"
struct NaiveTokenizer <: AbstractTokenizer end

"default behavior but counting the index"
struct NaiveIndexedTokenizer <: AbstractTokenizer end
tokenization(::NaiveIndexedTokenizer) = IndexedTokenization()


var documenterSearchIndex = {"docs":
[{"location":"api/#Api-reference","page":"Api reference","title":"Api reference","text":"","category":"section"},{"location":"api/","page":"Api reference","title":"Api reference","text":"","category":"page"},{"location":"api/","page":"Api reference","title":"Api reference","text":"Modules = [TextEncodeBase]","category":"page"},{"location":"api/#TextEncodeBase.AbstractTokenization","page":"Api reference","title":"TextEncodeBase.AbstractTokenization","text":"abstract type for tokenization.\n\nThe tokenization procedure is separate into multiple  TokenStages and recursive calls of splitting, wrap,  and tokenize. splitting break string into substrings,  wrap mark the substrings with new TokenStages, and  tokenize is responsible for the tokenization.\n\n\n\n\n\n","category":"type"},{"location":"api/#TextEncodeBase.AbstractTokenizer","page":"Api reference","title":"TextEncodeBase.AbstractTokenizer","text":"abstract type for tokenizers.\n\nEach tokenizer is link with a tokenization (by  defining tokenization(::Tokenizer) = Tokenization()).  The overall framework dispatch on both tokenizer and  tokenization, but most of the time we only add methods  for tokenization. This allow further composability and  can interfere the tokenization process with given  tokenizer.\n\n\n\n\n\n","category":"type"},{"location":"api/#TextEncodeBase.FlatTokenizer","page":"Api reference","title":"TextEncodeBase.FlatTokenizer","text":"tokenizer that return flat array instead of nested array of tokens\n\n\n\n\n\n","category":"type"},{"location":"api/#TextEncodeBase.NestedTokenizer","page":"Api reference","title":"TextEncodeBase.NestedTokenizer","text":"tokenizer that return nested array instead of flat array of tokens\n\n\n\n\n\n","category":"type"},{"location":"api/#TextEncodeBase.Splittability","page":"Api reference","title":"TextEncodeBase.Splittability","text":"splittability trait\n\nThe splittability trait decide whether the given combination (tokenizer x tokenization x stage) is  splittable or not (Splittable or UnSplittable). For example, DefaultTokenization and SentenceStage  is splittable (i.e. splittability(::DefaultTokenization, ::SentenceStage) = Splittable()). The splittability  change the behavior of tokenize: if it's splittable, tokenize will try to call splitting on the input,  wrap each splitting result and recurse. Otherwise, it will directly call wrap and then recurse into tokenize.\n\n\n\n\n\n","category":"type"},{"location":"api/#TextEncodeBase.TokenStages","page":"Api reference","title":"TextEncodeBase.TokenStages","text":"abstract type for type that wrap input into specific stage for control tokenization.\n\nThere are six builtin stages in TextEncodeBase (all abstract XStage <: TokenStages):\n\n1. Document <: DocumentStage: the input string is a full document,\n and thus need to be splitted into multiple sentence.\n2. Sentence <: SentenceStage: the input string is a full string,\n and thus need to be splitted into multiple part (SubSentence/Word/Token).\n3. SubSentence <: SubSentenceStage: special wrapper for case where the tokenizer\n does not directly break sentence all into words/tokens and these pieces contain\n multiple words/tokens, but you need the information that they are not full sentence.\n4. Word <: WordStage: the input string is a single word.\n5. SubWord <: SubWordStage: similar to SubSentence, but for word.\n6. Token <: TokenStage: the final piece of the tokenization process.\n Generally, it's used to specify the end of this piece and should\n never be splitted.\n\nEach wrapper have two field: x for the input, meta for extra information (nothing if not provided).\n\n\n\n\n\n","category":"type"},{"location":"api/#TextEncodeBase.Vocab","page":"Api reference","title":"TextEncodeBase.Vocab","text":"Vocab(data::Vector{<:AbstractString}, unk::AbstractString=\"[UNK]\")\n\nConstructor for Vocab. data is the list of vocabulary word, can be nonunique.  The actual list will be the unique version of data (i.e. vocab.list = unique(data)).  unk is the indicator word for all unknown words. unk can be either in or not in data,  depends on the use case.\n\n\n\n\n\n","category":"type"},{"location":"api/#TextEncodeBase.lookup","page":"Api reference","title":"TextEncodeBase.lookup","text":"lookup(v::Vocab, x)\n\nLookup x in v. lookup words depends on the type of x. If x is an integer,  return the x-th word on the vocabulary list (i.e. v.list[x]) and return the unknown word  if x is out-of-bound (v.unk). If x is a string, return the indice of x in the vocabulary  list (i.e findfirst(==(x), v.list) and return the unknown indice if x not found in the list.  If the unknown word v.unk is in the list, the unknown indice is its indice, otherwise 0.\n\nExample\n\njulia> vocab = Vocab([\"a\", \"b\", \"c\", \"a\", \"b\", \"c\"])\nVocab{String, StaticArrays.SizedVector{3, String, Vector{String}}}(size = 3, unk = [UNK], unki = 0)\n\njulia> vocab_unk = Vocab([\"a\", \"b\", \"xxx\"], \"xxx\")\nVocab{String, StaticArrays.SizedVector{3, String, Vector{String}}}(size = 3, unk = xxx, unki = 3)\n\njulia> lookup(vocab, \"b\")\n2\n\njulia> lookup(vocab, \"d\")\n0\n\njulia> lookup(vocab_unk, \"d\")\n3\n\njulia> lookup(vocab, 1)\n\"a\"\n\njulia> lookup(vocab, 10000)\n\"[UNK]\"\n\njulia> lookup(vocab_unk, 10000)\n\"xxx\"\n\n\n\n\n\n\n","category":"function"},{"location":"api/#TextEncodeBase.lookup-Tuple{Type{OneHot}, Vocab, Any}","page":"Api reference","title":"TextEncodeBase.lookup","text":"lookup(OneHot, v::Vocab, i)\n\nlookup i and convert into one-hot representation.\n\nExample\n\njulia> lookup(OneHot, vocab, \"a\")\n3-element OneHot{3}:\n 1\n 0\n 0\n\njulia> lookup(OneHot, vocab, [\"a\" \"b\"; \"c\" \"d\"])\n3x2x2 OneHotArray{3, 3, Matrix{OneHot{0x00000003}}}:\n[:, :, 1] =\n 1  0\n 0  0\n 0  1\n\n[:, :, 2] =\n 0  0\n 1  0\n 0  0\n\njulia> lookup(OneHot, vocab, 3)\nERROR: DomainError with c:\ncannot convert `lookup(::Vocab, 3)` = \"c\" into one-hot representation.\nStacktrace:\n[...]\n\n\n\n\n\n\n","category":"method"},{"location":"api/#TextEncodeBase.lookup-Tuple{Vocab, AbstractArray}","page":"Api reference","title":"TextEncodeBase.lookup","text":"lookup(v::Vocab, is::AbstractArray)\n\nrecursively lookup value from is\n\nExample\n\njulia> lookup(vocab, [\"b\", \"c\", \"a\", \"A\", \"[UNK]\"])\n5-element Vector{Int64}:\n 2\n 3\n 1\n 0\n 0\n\njulia> lookup(vocab, [1, \"a\", 0, \"A\", \"[UNK]\"])\n5-element Vector{Any}:\n  \"a\"\n 1\n  \"[UNK]\"\n 0\n 0\n\n\n\n\n\n\n","category":"method"},{"location":"api/#TextEncodeBase.lookup-Tuple{Vocab, OneHotArray}","page":"Api reference","title":"TextEncodeBase.lookup","text":"lookup(v::Vocab, i::OneHotArray)\n\nconvert the one-hot representation back into words.\n\nExample\n\njulia> lookup(OneHot, vocab, [\"a\" \"b\"; \"c\" \"d\"])\n3x2x2 OneHotArray{3, 3, Matrix{OneHot{0x00000003}}}:\n[:, :, 1] =\n 1  0\n 0  0\n 0  1\n\n[:, :, 2] =\n 0  0\n 1  0\n 0  0\n\njulia> lookup(vocab, ans)\n2×2 Matrix{String}:\n \"a\"  \"b\"\n \"c\"  \"[UNK]\"\n\n\n\n\n\n\n","category":"method"},{"location":"api/#TextEncodeBase.nested2batch-Tuple{Any}","page":"Api reference","title":"TextEncodeBase.nested2batch","text":"nested2batch(x)\n\nconvert nested array into single array\n\nExample\n\njulia> TextEncodeBase.nested2batch([[[1 2],[3 4]]])\n1×2×2×1 Array{Int64, 4}:\n[:, :, 1, 1] =\n 1  2\n\n[:, :, 2, 1] =\n 3  4\n\n\n\n\n\n\n","category":"method"},{"location":"api/#TextEncodeBase.preprocess-Tuple{AbstractTokenizer, TextEncodeBase.TokenStages}","page":"Api reference","title":"TextEncodeBase.preprocess","text":"preprocess(tkr::AbstractTokenizer, x)\n\nPreprocess the input x. This is only called during tkr(x).\n\n\n\n\n\n","category":"method"},{"location":"api/#TextEncodeBase.splittability","page":"Api reference","title":"TextEncodeBase.splittability","text":"splittability(args...)\n\nReturn the splittability (Splittable/UnSplittable) of given argument combination.  Overload to make a TokenStages splittable.\n\n\n\n\n\n","category":"function"},{"location":"api/#TextEncodeBase.splittable-Tuple","page":"Api reference","title":"TextEncodeBase.splittable","text":"splittable(args...)\n\nReturn true if the splittability of given argument combination is Splittable().\n\n\n\n\n\n","category":"method"},{"location":"api/#TextEncodeBase.splitting","page":"Api reference","title":"TextEncodeBase.splitting","text":"splitting(t::AbstractTokenization, x::TokenStages)\n\nSplit x given its tokenization stage. For example,  the default behavior of a document stage is splitting into  sentences (with WordTokenizers.split_sentences).\n\nOverload this method for custom tokenization.\n\n\n\n\n\n","category":"function"},{"location":"api/#TextEncodeBase.tokenization-Tuple{AbstractTokenizer}","page":"Api reference","title":"TextEncodeBase.tokenization","text":"tokenization(::AbstractTokenizer) :: AbstractTokenization\n\nReturn the tokenization object of given tokenizer.\n\n\n\n\n\n","category":"method"},{"location":"api/#TextEncodeBase.tokenize_procedure-Tuple{Any, Any, Any}","page":"Api reference","title":"TextEncodeBase.tokenize_procedure","text":"tokenization_procedure(tokenizer, tokenizaton, stage)\n\nThe procedure of tokenization (splitting + wrap + tokenize).\n\n\n\n\n\n","category":"method"},{"location":"api/#TextEncodeBase.trunc_and_pad-Tuple{Any, Integer, Any}","page":"Api reference","title":"TextEncodeBase.trunc_and_pad","text":"trunc_and_pad(x, n, pad)\n\ntruncate x to length n, otherwise add pad at the end of x until length equal n.  x can be either nested or single array (but the element type should not be subtype of abstract array).  if n is nothing, the largest length of the nested array will be used.\n\nExample\n\njulia> TextEncodeBase.trunc_and_pad(1:5, 7, -1)\n7-element Vector{Int64}:\n  1\n  2\n  3\n  4\n  5\n -1\n -1\n\njulia> TextEncodeBase.trunc_and_pad([1:5, 2:7], 7, -1)\n2-element Vector{Vector{Int64}}:\n [1, 2, 3, 4, 5, -1, -1]\n [2, 3, 4, 5, 6, 7, -1]\n\njulia> TextEncodeBase.trunc_and_pad([1:5, [2:7, [1:2]]], nothing, -1)\n2-element Vector{Vector}:\n [1, 2, 3, 4, 5, -1]\n Vector[[2, 3, 4, 5, 6, 7], [[1, 2, -1, -1, -1, -1]]]\n\n\n\n\n\n\n","category":"method"},{"location":"api/#TextEncodeBase.with_head_tail-Tuple{Any, Any, Any}","page":"Api reference","title":"TextEncodeBase.with_head_tail","text":"with_head_tail(x, head, tail)\n\nReturn [head; x; tail]. Ignored if head or tail is nothing.\n\nExample\n\njulia> TextEncodeBase.with_head_tail(1:5, -1, -2)\n7-element Vector{Int64}:\n -1\n  1\n  2\n  3\n  4\n  5\n -2\n\njulia> TextEncodeBase.with_head_tail([1:5, 2:3], -1, -2)\n2-element Vector{Vector{Int64}}:\n [-1, 1, 2, 3, 4, 5, -2]\n [-1, 2, 3, -2]\n\n\n\n\n\n\n","category":"method"},{"location":"api/#TextEncodeBase.wrap","page":"Api reference","title":"TextEncodeBase.wrap","text":"wrap(t::AbstractTokenization, s::TokenStages, x)\n\nMark the tokenization stage of x, which is part of the splitting result of s.  For example, if we are doing simple whitespace tokenization and at the sentence stage,  then x is just single word of s and thus return Word(x) (or Token(x)).  Skip if x is already a TokenStages. (this method only apply to splittable stages)\n\nOverload this method to control the tokenization process.\n\n\n\n\n\n","category":"function"},{"location":"api/#TextEncodeBase.wrap-Tuple{AbstractTokenization, TextEncodeBase.TokenStages}","page":"Api reference","title":"TextEncodeBase.wrap","text":"wrap(t::AbstractTokenization, x::TokenStages)\n\nA handler for unsplittable stages (token/word/...).\n\nOverload this method for custom transform.\n\n\n\n\n\n","category":"method"},{"location":"design/#Design","page":"Design","title":"Design","text":"","category":"section"},{"location":"design/#Tokenizer","page":"Design","title":"Tokenizer","text":"","category":"section"},{"location":"design/","page":"Design","title":"Design","text":"The overall tokenizer framework is built on top of Julia's multiple dispatch.  The main idea of the design is to make hijacking the tokenization process easier.  This is done by dispatching to all AbstractTokenizer, AbstractTokenization, and  TokenStages, so that even if the tokenization and input are the same, we can still  define a new tokenizer and change the behavior of some parts of that tokenization.","category":"page"},{"location":"design/#TokenStages","page":"Design","title":"TokenStages","text":"","category":"section"},{"location":"design/","page":"Design","title":"Design","text":"The TokenStages is an abstract type used to specify the input. For example, we have  Document <: TokenStages and Sentence <: TokenStages, so the input is not just a  String, which we probably cannot detect what is in. Every string should be wrap  with a TokenStages type explicitly. With the stages in mind, we can convert the  tokenization process into recursively splitting the string and wrapping the substring  as another stage until the result is a Token type.","category":"page"},{"location":"design/#Splittability","page":"Design","title":"Splittability","text":"","category":"section"},{"location":"design/","page":"Design","title":"Design","text":"Not every TokenStages can be splitted into substring, like most of tokenizer won't split  word into subwords. Therefore, we defined the Splittability trait. The splittability is  codetermined by AbstractTokenizer, AbstractTokenization, and TokenStages. It is either  Splittable or UnSplittable. If the input is splittable, there should have a splitting  method defined for that combination. On the other hand, if it's unsplittable, the tokenize  function will directly call wrap to tranform the input into next stage. Actually, there is  also another input with type (ParentStages = Union{Nothing, TokenStages) that can be used  to find whether the tokenize function is called recursively.","category":"page"},{"location":"design/#Vocabulary","page":"Design","title":"Vocabulary","text":"","category":"section"},{"location":"design/","page":"Design","title":"Design","text":"The Vocab type take two argument, the list of words and a special token for all unknown words.  The default constructor of Vocab copy the list and remove all duplicate words. Besides, it  also try to find the unknown token in the word list. If the unknown token is NOT in the word list,  it will NOT add it into the word list. Instead, when lookup unknown word with that Vocab object,  it will return 0 as the index for all unknown words. Therefore, make sure the unknown token is in the  word list beforehand.","category":"page"},{"location":"","page":"TextEncodeBase","title":"TextEncodeBase","text":"CurrentModule = TextEncodeBase","category":"page"},{"location":"#TextEncodeBase","page":"TextEncodeBase","title":"TextEncodeBase","text":"","category":"section"},{"location":"","page":"TextEncodeBase","title":"TextEncodeBase","text":"Documentation for TextEncodeBase.","category":"page"},{"location":"","page":"TextEncodeBase","title":"TextEncodeBase","text":"An api for encoding text, built on top of WordTokenizers.jl.  Providing a framework to easily define custom methods to convert strings into indices.","category":"page"},{"location":"#Usages","page":"TextEncodeBase","title":"Usages","text":"","category":"section"},{"location":"","page":"TextEncodeBase","title":"TextEncodeBase","text":"Here are some explanation and examples for using TextEncodeBase.jl, you can also find other information  from the test","category":"page"},{"location":"#Vocabulary","page":"TextEncodeBase","title":"Vocabulary","text":"","category":"section"},{"location":"","page":"TextEncodeBase","title":"TextEncodeBase","text":"The vocabulary part contains only two api, the Vocab struct and the lookup function.  The lookup function is bidirectional (convert string to indices and back).","category":"page"},{"location":"","page":"TextEncodeBase","title":"TextEncodeBase","text":"julia> vocab = Vocab([\"a\", \"b\", \"c\", \"a\", \"b\", \"c\"])\nVocab{String, StaticArrays.SizedVector{3, String, Vector{String}}}(size = 3, unk = [UNK], unki = 0)\n\njulia> vocab_unk = Vocab([\"a\", \"b\", \"xxx\"], \"xxx\")\nVocab{String, StaticArrays.SizedVector{3, String, Vector{String}}}(size = 3, unk = xxx, unki = 3)\n\njulia> lookup(vocab, \"b\")\n2\n\njulia> lookup(vocab, \"d\")\n0\n\njulia> lookup(vocab_unk, \"d\")\n3\n\njulia> lookup(vocab, 1)\n\"a\"\n\njulia> lookup(vocab, 10000)\n\"[UNK]\"\n\njulia> lookup(vocab_unk, 10000)\n\"xxx\"\n\njulia> lookup(vocab, [\"b\", \"c\", \"a\", \"A\", \"[UNK]\"])\n5-element Vector{Int64}:\n 2\n 3\n 1\n 0\n 0\n\njulia> lookup(OneHot, vocab, \"a\")\n3-element OneHot{3}:\n 1\n 0\n 0\n\njulia> lookup(OneHot, vocab, 3)\nERROR: DomainError with c:\ncannot convert `lookup(::Vocab, 3)` = \"c\" into one-hot representation.\nStacktrace:\n[...]\n\njulia> oha = lookup(OneHot, vocab, [\"a\" \"b\"; \"c\" \"d\"])\n3x2x2 OneHotArray{3, 3, Matrix{OneHot{0x00000003}}}:\n[:, :, 1] =\n 1  0\n 0  0\n 0  1\n\n[:, :, 2] =\n 0  0\n 1  0\n 0  0\n\njulia> lookup(vocab, oha)\n2×2 Matrix{String}:\n \"a\"  \"b\"\n \"c\"  \"[UNK]\"\n","category":"page"},{"location":"#Tokenizer","page":"TextEncodeBase","title":"Tokenizer","text":"","category":"section"},{"location":"","page":"TextEncodeBase","title":"TextEncodeBase","text":"The tokenizer part is built ontop of WordTokenizers.jl and provide a high-level api  to control/augment the tokenization. There're some differences between WordTokenizers.jl.  WordTokenizers.jl provides a set of tokenizers and a low-level api (TokenBuffer) for define  custom tokenizers. It's mainly focus on how to split a setnece into tokens. We, on the other hand,  focus on how to combine different tokenizer or include other information during the tokenization.  For example, sometimes you might want to prevent urls from being splited or add some extra tags to it,  these can be done by defining a custom AbstractTokenizer and overload some methods. Besides, we  force the user to explicit wrap the input as one of the stages (Document/Sentence/Word/...),  so no confusion.","category":"page"},{"location":"#Example-of-using-the-Tokenizer-api","page":"TextEncodeBase","title":"Example of using the Tokenizer api","text":"","category":"section"},{"location":"","page":"TextEncodeBase","title":"TextEncodeBase","text":"Here is an example that wrapped the word tokenizer and wordpiece from Transformers.jl into our Tokenizer api.","category":"page"},{"location":"","page":"TextEncodeBase","title":"TextEncodeBase","text":"using Transformers\nusing Transformers.Pretrain\nusing Transformers.BidirectionalEncoder: WordPiece, bert_cased_tokenizer\n\nusing TextEncodeBase\nusing TextEncodeBase: NestedTokenizer, BaseTokenization, Sentence, Word, SubWord, getvalue, Splittable\n\nstruct BertCasedTokenization <: BaseTokenization\n    wordpiece::WordPiece\nend\n\n# split sentence with `bert_cased_tokenizer` (define with WordTokenizers.jl's `TokenBuffer`)\nTextEncodeBase.splitting(::BertCasedTokenization, s::Sentence) = bert_cased_tokenizer(getvalue(s))\n\n# word is splittable with WordPiece\nTextEncodeBase.splittability(::BertCasedTokenization, w::Word) = Splittable()\n\n# split word with `WordPiece`\nTextEncodeBase.splitting(t::BertCasedTokenization, w::Word) = t.wordpiece(getvalue(w))\n\ntokenizer = pretrain\"bert-cased_L-12_H-768_A-12:tokenizer\" # this is just `bert_cased_tokenizer`\nwordpiece = pretrain\"bert-cased_L-12_H-768_A-12:wordpiece\"\n\ntkr = NestedTokenizer(BertCasedTokenization(wordpiece))\n\ntext1 = \"Peter Piper picked a peck of pickled peppers\"\nsingle_without_TEB = text1 |> tokenizer |> wordpiece\nsingle_with_TEB = tkr(Sentence(text1))\n\n# `NestedTokenizer` return vector of vector\n@assert single_without_TEB == map(getvalue, single_with_TEB[])\n\njulia> single_without_TEB\n11-element Vector{String}:\n \"Peter\"\n \"Piper\"\n \"picked\"\n \"a\"\n \"p\"\n \"##eck\"\n \"of\"\n \"pick\"\n \"##led\"\n \"pepper\"\n \"##s\"\n\njulia> single_with_TEB\n1-element Vector{Vector{TextEncodeBase.TokenStage}}:\n [Token(\"Peter\"), Token(\"Piper\"), Token(\"picked\"), Token(\"a\"), Token(\"p\"), Token(\"##eck\"), Token(\"of\"), Token(\"pick\"), Token(\"##led\"), Token(\"pepper\"), Token(\"##s\")]\n\njulia> single_without_TEB == map(getvalue, single_with_TEB[])\ntrue\n\n\n# define stage for batch of data\nstruct BatchSentence{A<:AbstractVector, M} <: TextEncodeBase.DocumentStage\n    x::A\n    meta::M\nend\n\nBatchSentence(x) = BatchSentence(x, nothing)\nTextEncodeBase.setmeta(x::BatchSentence, meta) = BatchSentence(x.x, meta)\nTextEncodeBase.setvalue(x::BatchSentence, y) = BatchSentence(y, x.meta)\n\n# splittability and split behavior for `BatchSentence`\nTextEncodeBase.splittability(::BertCasedTokenization, ::BatchSentence) = Splittable()\nTextEncodeBase.splitting(::BertCasedTokenization, s::BatchSentence) = s.x\n\ntext2 = \"Fuzzy Wuzzy was a bear\"\ntexts = [text1, text2]\n\nbatch_without_TEB = map(wordpiece∘tokenizer, texts)\nbatch_with_TEB = tkr(BatchSentence(texts))\n\n@assert batch_without_TEB == TextEncodeBase.nestedcall(getvalue, batch_with_TEB)\n\njulia> batch_without_TEB\n2-element Vector{Vector{String}}:\n [\"Peter\", \"Piper\", \"picked\", \"a\", \"p\", \"##eck\", \"of\", \"pick\", \"##led\", \"pepper\", \"##s\"]\n [\"Fu\", \"##zzy\", \"Wu\", \"##zzy\", \"was\", \"a\", \"bear\"]\n\njulia> batch_with_TEB\n2-element Vector{Vector{TextEncodeBase.TokenStage}}:\n [Token(\"Peter\"), Token(\"Piper\"), Token(\"picked\"), Token(\"a\"), Token(\"p\"), Token(\"##eck\"), Token(\"of\"), Token(\"pick\"), Token(\"##led\"), Token(\"pepper\"), Token(\"##s\")]\n [Token(\"Fu\"), Token(\"##zzy\"), Token(\"Wu\"), Token(\"##zzy\"), Token(\"was\"), Token(\"a\"), Token(\"bear\")]\n\njulia> batch_without_TEB == TextEncodeBase.nestedcall(getvalue, batch_with_TEB)\ntrue\n","category":"page"},{"location":"","page":"TextEncodeBase","title":"TextEncodeBase","text":"Since the wordpiece break word into subword, we might want to know which word each subword belongs to:","category":"page"},{"location":"","page":"TextEncodeBase","title":"TextEncodeBase","text":"julia> itkr = NestedTokenizer(TextEncodeBase.IndexedTokenization(BertCasedTokenization(wordpiece)));\n\njulia> ibatch_with_TEB = itkr(BatchSentence(texts));\n\n# subword from same word having the same `word_id`\njulia> ibatch_with_TEB[1]\n11-element Vector{TextEncodeBase.TokenStage}:\n Token(\"Peter\", (sentence_id = 1, word_id = 1, token_id = 1))\n Token(\"Piper\", (sentence_id = 1, word_id = 2, token_id = 2))\n Token(\"picked\", (sentence_id = 1, word_id = 3, token_id = 3))\n Token(\"a\", (sentence_id = 1, word_id = 4, token_id = 4))\n Token(\"p\", (sentence_id = 1, word_id = 5, token_id = 5))\n Token(\"##eck\", (sentence_id = 1, word_id = 5, token_id = 6))\n Token(\"of\", (sentence_id = 1, word_id = 6, token_id = 7))\n Token(\"pick\", (sentence_id = 1, word_id = 7, token_id = 8))\n Token(\"##led\", (sentence_id = 1, word_id = 7, token_id = 9))\n Token(\"pepper\", (sentence_id = 1, word_id = 8, token_id = 10))\n Token(\"##s\", (sentence_id = 1, word_id = 8, token_id = 11))\n\njulia> ibatch_with_TEB[2]\n7-element Vector{TextEncodeBase.TokenStage}:\n Token(\"Fu\", (sentence_id = 2, word_id = 1, token_id = 1))\n Token(\"##zzy\", (sentence_id = 2, word_id = 1, token_id = 2))\n Token(\"Wu\", (sentence_id = 2, word_id = 2, token_id = 3))\n Token(\"##zzy\", (sentence_id = 2, word_id = 2, token_id = 4))\n Token(\"was\", (sentence_id = 2, word_id = 3, token_id = 5))\n Token(\"a\", (sentence_id = 2, word_id = 4, token_id = 6))\n Token(\"bear\", (sentence_id = 2, word_id = 5, token_id = 7))\n","category":"page"},{"location":"#TextEncoder","page":"TextEncodeBase","title":"TextEncoder","text":"","category":"section"},{"location":"","page":"TextEncodeBase","title":"TextEncodeBase","text":"The text encoder is just a combination of vocabulary and tokenizer. We also  provide some helper function like (with_head_tail/nested2batch/...) for  transform the tokenizer result into lookup-able format.","category":"page"},{"location":"#Example","page":"TextEncodeBase","title":"Example","text":"","category":"section"},{"location":"","page":"TextEncodeBase","title":"TextEncodeBase","text":"using TextEncodeBase: nestedcall, with_head_tail, trunc_and_pad, nested2batch\n\n# construct `Vocab` with `WordPiece`\nvocab = Vocab(wordpiece.vocab, wordpiece.vocab[wordpiece.unk_idx])\n\n# define encoder with `TextEncoder`\nencoder = TextEncoder(\n    itkr, vocab,\n    nested2batch ∘ trunc_and_pad(nothing, vocab.unk) ∘ with_head_tail(\"[CLS]\", \"[SEP]\") ∘ nestedcall(getvalue)\n)\n\njulia> encode(enc, BatchSentence(texts))\n28996x13x2 OneHotArray{28996, 3, Matrix{OneHot{0x00007144}}}:\n[...]\n\njulia> decode(enc, ans)\n13×2 Matrix{String}:\n \"[CLS]\"   \"[CLS]\"\n \"Peter\"   \"Fu\"\n \"Piper\"   \"##zzy\"\n \"picked\"  \"Wu\"\n \"a\"       \"##zzy\"\n \"p\"       \"was\"\n \"##eck\"   \"a\"\n \"of\"      \"bear\"\n \"pick\"    \"[SEP]\"\n \"##led\"   \"[UNK]\"\n \"pepper\"  \"[UNK]\"\n \"##s\"     \"[UNK]\"\n \"[SEP]\"   \"[UNK]\"\n","category":"page"},{"location":"#Outline","page":"TextEncodeBase","title":"Outline","text":"","category":"section"},{"location":"","page":"TextEncodeBase","title":"TextEncodeBase","text":"Pages = [\n\t\"design.md\",\n\t\"api.md\",\n]","category":"page"}]
}

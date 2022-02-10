# Design

## Tokenizer

The overall tokenizer framework is built on top of Julia's multiple dispatch.
 The main idea of the design is to make hijacking the tokenization process easier.
 This is done by dispatching to all `AbstractTokenizer`, `AbstractTokenization`, and
 `TokenStages`, so that even if the tokenization and input are the same, we can still
 define a new tokenizer and change the behavior of some parts of that tokenization.

### TokenStages

The `TokenStages` is an abstract type used to specify the input. For example, we have
 `Document <: TokenStages` and `Sentence <: TokenStages`, so the input is not just a
 `String`, which we probably cannot detect what is in. Every string should be wrap
 with a `TokenStages` type explicitly. With the stages in mind, we can convert the
 tokenization process into recursively splitting the string and wrapping the substring
 as another stage until the result is a `Token` type.

### Splittability

Not every `TokenStages` can be splitted into substring, like most of tokenizer won't split
 word into subwords. Therefore, we defined the `Splittability` trait. The splittability is
 codetermined by `AbstractTokenizer`, `AbstractTokenization`, and `TokenStages`. It is either
 `Splittable` or `UnSplittable`. If the input is splittable, there should have a `splitting`
 method defined for that combination. On the other hand, if it's unsplittable, the tokenize
 function will directly call `wrap` to tranform the input into next stage. Actually, there is
 also another input with type (`ParentStages = Union{Nothing, TokenStages`) that can be used
 to find whether the tokenize function is called recursively.


## Vocabulary

The `Vocab` type take two argument, the list of words and a special token for all unknown words.
 The default constructor of `Vocab` copy the list and remove all duplicate words. Besides, it
 also try to find the unknown token in the word list. If the unknown token is *NOT* in the word list,
 it will *NOT* add it into the word list. Instead, when `lookup` unknown word with that `Vocab` object,
 it will return 0 as the index for all unknown words. Therefore, make sure the unknown token is in the
 word list beforehand.


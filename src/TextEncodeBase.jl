module TextEncodeBase

import WordTokenizers

"""
abstract type for tokenizers.

Each tokenizer is link with a tokenization (by
 defining `tokenization(::Tokenizer) = Tokenization()`).
 The overall framework dispatch on both tokenizer and
 tokenization, but most of the time we only add methods
 for tokenization. This allow better composability and
 can interfere the tokenization process with given
 tokenizer.
"""
abstract type AbstractTokenizer end

"""
abstract type for tokenization.

The tokenization procedure is separate into multiple
 `TokenStages` and recursive calls of `splitting` and
 `tokenize`. `splitting` break string into substrings,
 and `tokenize` is responsible for marking each substring
 with a `TokenStages` and do the tokenization.
"""
abstract type AbstractTokenization end

struct DefaultTokenization <: AbstractTokenization end

tokenization(::AbstractTokenizer) = DefaultTokenization()

include("./base.jl")
include("./indexed.jl")
# include("./match.jl")
include("tkrs.jl")

end

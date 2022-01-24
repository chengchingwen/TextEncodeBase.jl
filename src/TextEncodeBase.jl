module TextEncodeBase

import WordTokenizers

"""
abstract type for tokenizers.

Each tokenizer is link with a tokenization (by
 defining `tokenization(::Tokenizer) = Tokenization()`).
 The overall framework dispatch on both tokenizer and
 tokenization, but most of the time we only add methods
 for tokenization. This allow further composability and
 can interfere the tokenization process with given
 tokenizer.
"""
abstract type AbstractTokenizer end

"""
abstract type for tokenization.

The tokenization procedure is separate into multiple
 `TokenStages` and recursive calls of `splitting`, `wrap`,
 and `tokenize`. `splitting` break string into substrings,
 `wrap` mark the substrings with new `TokenStages`, and
 `tokenize` is responsible for the tokenization.
"""
abstract type AbstractTokenization end

struct DefaultTokenization <: AbstractTokenization end

tokenization(::AbstractTokenizer) = DefaultTokenization()

include("./utils.jl")
include("./base.jl")
include("./indexed.jl")
include("./match.jl")
include("./tkrs.jl")

end

module TextEncodeBase

import WordTokenizers

"""
abstract type for tokenizers.

The tokenization procedure is separate into multiple
 `TokenStages` and recursive calls of `splitting` 
 and `tokenize`. `splitting` break string into substrings,
 and `tokenize` is responsible for marking the
 `TokenStages` and do the recursive call.
"""
abstract type AbstractTokenizer end

include("./base.jl")
include("./indexed.jl")
# include("./match.jl")

struct NaiveTokenizer <: AbstractTokenizer end


"""
    splitting(tkr::AbstractTokenizer, x::TokenStages)

Split `x` given its tokenization stage. For example,
 the default behavior of a document stage is splitting into
 sentences (with `WordTokenizers.split_sentences`).

Overload this method for custom tokenizer.
"""
function splitting end

"""
    tokenize(tkr::AbstractTokenizer, s::TokenStages, x)

Tokenize `x`. `s` contain the parent of `x`.

Overload this method when you need parent information to
 tokenize `x`.
"""
function tokenize end

@eval $((@macroexpand @doc """
    tokenize(tkr::AbstractTokenizer, x::TokenStages)

Tokenize `x`. 

Overload this method for custom tokenizer and stages.
"""
function tokenize(t::AT, x::TS) where {AT <: AbstractTokenizer, TS <: TokenStages} end
).args[2])

end

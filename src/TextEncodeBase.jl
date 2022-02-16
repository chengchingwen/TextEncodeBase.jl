module TextEncodeBase

using PartialFunctions
import WordTokenizers

using PrimitiveOneHot
using PrimitiveOneHot: OneHot

# tokenize
export AbstractTokenizer, AbstractTokenization

include("./utils.jl")
include("./base.jl")
include("./indexed.jl")
include("./match.jl")
include("./tkrs.jl")

# vocab
export AbstractVocabulary, Vocab, lookup, OneHot, OneHotArray

include("./vocab.jl")

# encode
export AbstractTextEncoder, TextEncoder, encode, decode

include("./encode.jl")

end

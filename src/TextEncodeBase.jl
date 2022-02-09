module TextEncodeBase

using PartialFunctions
import WordTokenizers

using PrimitiveOneHot
using PrimitiveOneHot: OneHot

# tokenize
export AbstractTokenizer

include("./utils.jl")
include("./base.jl")
include("./hie.jl")
include("./indexed.jl")
include("./match.jl")
include("./tkrs.jl")

# vocab
export Vocab, lookup, OneHot, OneHotArray

include("./vocab.jl")

# encode
export AbstractTextEncoder, TextEncoder

include("./encode.jl")

end

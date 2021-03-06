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
include("./batch.jl")
include("./macro.jl")
include("./normalize.jl")

# vocab
export AbstractVocabulary, Vocab, lookup, OneHot, OneHotArray

include("./vocab.jl")

# reexport pipeline
using FuncPipelines
export Pipeline, Pipelines, PipeGet

# encode
export AbstractTextEncoder, TextEncoder, encode, decode

include("./encode.jl")



end

module TextEncodeBase

import WordTokenizers

# tokenize
include("./utils.jl")
include("./base.jl")
include("./indexed.jl")
include("./match.jl")
include("./tkrs.jl")

# vocab
include("./vocab.jl")

end

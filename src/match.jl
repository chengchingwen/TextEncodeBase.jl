struct MatchTokenization <: AbstractTokenization
    patterns::Vector{Regex}
end

@noinline splitting(t::MatchTokenization, s::SentenceStage) = collect(Tuple{Bool, SubString}, matchsplits(t.patterns, s.x))

@inline tokenize(t::MatchTokenization, s::SentenceStage, (istoken, x)) = istoken ? Token(x, s.meta) : SubSentence(x, s.meta)

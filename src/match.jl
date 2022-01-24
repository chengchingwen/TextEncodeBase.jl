struct MatchTokenization <: AbstractTokenization
    patterns::Vector{Regex}
end

@noinline splitting(t::MatchTokenization, s::SentenceStage) = collect(Tuple{Bool, SubString}, matchsplits(t.patterns, getvalue(s)))
@inline wrap(t::MatchTokenization, s::SentenceStage, (istoken, x)) = istoken ? Token(x, getmeta(s)) : SubSentence(x, getmeta(s))

# calling directly on word should check if any match exists
@noinline splitting(t::MatchTokenization, w::WordStage) = collect(Tuple{Bool, SubString}, matchsplits(t.patterns, getvalue(w)))
@inline tokenize(tkr::AbstractTokenizer, t::MatchTokenization, ::Nothing, w::WordStage) = tokenize_procedure(tkr, t, w)
@inline wrap(t::MatchTokenization, w::WordStage, (istoken, x)) = istoken ? Token(x, getmeta(w)) : Word(x, getmeta(w))

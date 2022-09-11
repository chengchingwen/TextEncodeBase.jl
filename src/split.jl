struct EachSplitTokenization{S} <: BaseTokenization
    splitter::S
end

splitting(t::EachSplitTokenization, s::SentenceStage) = eachsplit(getvalue(s), t.splitter)

struct EachMatchTokenization <: BaseTokenization
    pattern::Regex
    EachMatchTokenization(r) = new(Base.compile(as_match(r)))
end

splitting(t::EachMatchTokenization, s::SentenceStage) = Iterators.map(x->String(x.match), eachmatch(t.pattern, getvalue(s)))

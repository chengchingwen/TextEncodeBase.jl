struct EachSplitTokenization{S} <: BaseTokenization
    splitter::S
end

@static if VERSION < v"1.8"
    splitting(t::EachSplitTokenization, s::SentenceStage) = split(getvalue(s), t.splitter; keepempty = false)
else
    splitting(t::EachSplitTokenization, s::SentenceStage) = eachsplit(getvalue(s), t.splitter; keepempty = false)
end

struct EachMatchTokenization <: BaseTokenization
    pattern::Regex
    EachMatchTokenization(r) = new(Base.compile(as_match(r)))
end

splitting(t::EachMatchTokenization, s::SentenceStage) = Iterators.map(x->String(x.match), eachmatch(t.pattern, getvalue(s)))

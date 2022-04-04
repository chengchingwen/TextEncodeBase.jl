using TextEncodeBase
using Test

# quick and dirty macro for making @inferred as test case
macro test_inferred(ex)
    esc(quote
        @test begin
            @inferred $ex
            true
        end
    end)
end

using TextEncodeBase: AbstractTokenizer, AbstractTokenization,
    BaseTokenization, NestedTokenizer, FlatTokenizer,
    WordTokenization, IndexedTokenization, MatchTokenization,
    TokenStages, Document, Sentence, Word, Token, Batch
using TextEncodeBase: getvalue, getmeta, with_head_tail, trunc_and_pad, nested2batch, nestedcall

using WordTokenizers

const ATR = AbstractTokenizer
const AT = AbstractTokenization
const BT = BaseTokenization

struct CharTk <: BT end
TextEncodeBase.splitting(::CharTk, x::Word) = split(x.x, "")
TextEncodeBase.splittability(::CharTk, x::Word) = TextEncodeBase.Splittable()

@testset "TextEncodeBase.jl" begin
    @testset "Tokenize" begin
        document = Document("This is the first sentence. And the second one with some number 12345.")
        sentence = Sentence("A single sentence with 31 char.")
        word = Word("word")

        @testset "base tokenizer" begin
            tkr = FlatTokenizer()
            @test tkr(document) == map(Token, mapfoldl(nltk_word_tokenize, append!, split_sentences(document.x)))
            @test tkr(sentence) == map(Token, nltk_word_tokenize(sentence.x))
            @test tkr(word) == [Token(word.x)]
        end

        @testset "edge case" begin
            tkr = FlatTokenizer()
            @test tkr(Document("")) == []
            @test tkr(Sentence("")) == []
            @test tkr(Word("")) == []
            @test tkr(Token("")) == []
        end

        @testset "word tokenizer" begin
            tkr = FlatTokenizer(WordTokenization(tokenize=poormans_tokenize))
            tkr2 = FlatTokenizer()
            @test tkr(document) == map(Token, mapfoldl(poormans_tokenize, append!, split_sentences(document.x)))
            @test tkr(sentence) == map(Token, poormans_tokenize(sentence.x))
            @test tkr(word) == [Token(word.x)]
            @test tkr(document) != tkr2(document)
            @test tkr(sentence) != tkr2(sentence)
        end

        @testset "index tokenizer" begin
            tkr = FlatTokenizer(IndexedTokenization())
            @test tkr(document) == begin
                sentences = split_sentences(document.x)
                words = map(x->nltk_word_tokenize(x), sentences)
                tokens = Token[]
                for (i, s) in enumerate(words)
                    for (j, w) in enumerate(s)
                        push!(tokens, Token(w, (sentence_id = i, word_id = j, token_id = j)))
                    end
                end
                tokens
            end
            @test tkr(sentence) == begin
                words = nltk_word_tokenize(sentence.x)
                tokens = Token[]
                for (i, w) in enumerate(words)
                    push!(tokens, Token(w, (word_id = i, token_id = i)))
                end
                tokens
            end
            @test tkr(word) == [Token(word.x, (word_id = 1, token_id  = 1))]
        end

        @testset "match tokenizer" begin
            tkr = FlatTokenizer(MatchTokenization([r"\d", r"en"]))
            @test map(getvalue, tkr(document)) == [
                "This", "is", "the", "first", "s",
                "en", "t", "en", "ce", ".", "And",
                "the", "second", "one", "with", "some",
                "number", "1", "2", "3", "4", "5", ".",
            ]
            @test map(getvalue, tkr(sentence)) == [
                "A", "single", "s", "en", "t", "en",
                "ce", "with", "3", "1", "char", ".",
            ]
            @test map(getvalue, tkr(word)) == [word.x]
            @test map(getvalue, tkr(Word("123"))) == ["1", "2", "3"]
        end

        @testset "indexed match tokenizer" begin
            tkr = FlatTokenizer(IndexedTokenization(MatchTokenization([r"\d", r"en"])))
            @test map(getvalue, tkr(document)) == [
                "This", "is", "the", "first", "s",
                "en", "t", "en", "ce", ".", "And",
                "the", "second", "one", "with", "some",
                "number", "1", "2", "3", "4", "5", ".",
            ]
            @test map(getmeta, tkr(document)) == begin
                s = Iterators.flatten((Iterators.repeated(1, 10), Iterators.repeated(2, 13)))
                w = Iterators.flatten((1:10, 1:13))
                map(NamedTuple{(:sentence_id, :word_id, :token_id)}, zip(s, w, w))
            end

            @test map(getvalue, tkr(sentence)) == [
                "A", "single", "s", "en", "t", "en",
                "ce", "with", "3", "1", "char", ".",
            ]
            @test map(getmeta, tkr(sentence)) == map(NamedTuple{(:word_id, :token_id)}, zip(1:12, 1:12))
            @test map(getvalue, tkr(word)) == [word.x]
            @test map(getmeta, tkr(word)) == [(word_id = 1, token_id = 1)]
            @test map(getvalue, tkr(Word("123"))) == ["1", "2", "3"]
            @test map(getmeta, tkr(Word("123"))) == map(NamedTuple{(:word_id, :token_id)}, zip(1:3, 1:3))
        end

        @testset "nested output" begin
            tkr = NestedTokenizer(IndexedTokenization())
            @test tkr(document) == begin
                sentences = split_sentences(document.x)
                words = map(x->nltk_word_tokenize(x), sentences)
                tokens = []
                for (i, s) in enumerate(words)
                    push!(tokens, map(enumerate(s)) do (j, w)
                          Token(w, (sentence_id = i, word_id = j, token_id = j))
                          end)
                end
                tokens
            end
            @test tkr(sentence) == begin
                words = nltk_word_tokenize(sentence.x)
                tokens = [map(enumerate(words)) do (i, w)
                          Token(w, (word_id = i, token_id = i))
                          end]
                tokens
            end
            @test tkr(word) == [Token(word.x, (word_id = 1, token_id  = 1))]
        end

        @testset "indexed char" begin
            tkr = FlatTokenizer(IndexedTokenization(CharTk()))
            @test tkr(document) == begin
                sentences = split_sentences(document.x)
                words = map(x->nltk_word_tokenize(x), sentences)
                tokens = Token[]
                for (i, s) in enumerate(words)
                    k = 1
                    for (j, w) in enumerate(s)
                        for c in split(w, "")
                            push!(tokens, Token(c, (sentence_id = i, word_id = j, token_id = k)))
                            k += 1
                        end
                    end
                end
                tokens
            end
            @test tkr(sentence) == begin
                words = nltk_word_tokenize(sentence.x)
                tokens = Token[]
                for (i, w) in enumerate(words)
                    for c in split(w, "")
                        push!(tokens, Token(c, (word_id = i, token_id = length(tokens)+1)))
                    end
                end
                tokens
            end
            @test tkr(word) == begin
                chars = split(word.x, "")
                tokens = Token[]
                for (i, c) in enumerate(chars)
                    push!(tokens, Token(c, (word_id = 1, token_id = i)))
                end
                tokens
            end
        end

        @testset "nested indexed match char" begin
            tkr = NestedTokenizer(IndexedTokenization(MatchTokenization(CharTk(), [r"\d", r"en"])))
            s(x) = split(x, "")
            r(x, n) = repeat(x:x, n)
            @test nestedcall(getvalue, tkr(document)) == [
                [
                    s("This"); s("is"); s("the"); s("first");
                    "s"; "en"; "t"; "en"; s("ce"); ".";
                ],
                [
                    s("And"); s("the"); s("second"); s("one"); s("with");
                    s("some"); s("number"); "1"; "2"; "3"; "4"; "5"; ".";
                ]
            ]
            @test nestedcall(getmeta, tkr(document)) == begin
                sentence_id = [r(1, 21), r(2, 35)]
                word_id = [[
                        r(1, 4); r(2, 2); r(3, 3); r(4, 5);
                        5; 6; 7; 8; r(9, 2); 10;
                    ], [
                        r(1, 3); r(2, 3); r(3, 6); r(4, 3); r(5, 4);
                        r(6, 4); r(7, 6); 8; 9; 10; 11; 12; 13;
                    ]]
                token_id = [[1:21;], [1:35;]]
                map((s,w,t)->map(NamedTuple{(:sentence_id, :word_id, :token_id)}, zip(s,w,t)), sentence_id, word_id, token_id)
            end
            @test nestedcall(getvalue, tkr(sentence)) == [
                [
                    "A"; s("single"); "s"; "en"; "t"; "en";
                    s("ce"); s("with"); "3"; "1"; s("char"); ".";
                ]
            ]
            @test nestedcall(getmeta, tkr(sentence)) == begin
                word_id = [
                    1; r(2, 6); 3; 4; 5; 6;
                    r(7, 2); r(8, 4); 9; 10; r(11, 4); 12;
                ]
                token_id = [1:24;]
                [map(NamedTuple{(:word_id, :token_id)}, zip(word_id, token_id))]
            end
            @test nestedcall(getvalue, tkr(word)) == ["w", "o", "r", "d"]
            @test nestedcall(getmeta, tkr(word)) == map(NamedTuple{(:word_id, :token_id)}, zip(r(1, 4), 1:4))
        end

        @testset "@stage" begin
            @test_throws Exception @macroexpand(TextEncodeBase.@stage SomeStage{A})
            @test_throws Exception @macroexpand(TextEncodeBase.@stage SomeStage{A, B, C})
            @test_throws Exception @macroexpand(TextEncodeBase.@stage SomeStage{A, B} <: C D)
            @test_throws Exception @macroexpand(TextEncodeBase.@stage 3)
            @test_throws Exception @macroexpand(TextEncodeBase.@stage SomeStage{A}())

            @test_nowarn @macroexpand(TextEncodeBase.@stage SomeStage)
            @test_nowarn @macroexpand(TextEncodeBase.@stage SomeStage{A, B} <: TokenStages)
        end

        @testset "batch" begin
            tkr = NestedTokenizer(IndexedTokenization())
            document = document.x
            sentence = sentence.x
            another_sentence = "This is another sentence"
            batch_sentence = [split_sentences(document); sentence; another_sentence]
            batch_document = [document, sentence, another_sentence]
            @test nestedcall(getvalue, tkr(Batch{Sentence}(batch_sentence))) == nltk_word_tokenize.(batch_sentence)
            @test nestedcall(getmeta, tkr(Batch{Sentence}(batch_sentence))) ==
                map(enumerate(nltk_word_tokenize.(batch_sentence))) do (i, v)
                    map(enumerate(v)) do (j, x)
                        (sentence_id = i, word_id = j, token_id = j)
                    end
                end
            @test nestedcall(getvalue, tkr(Batch{Document}(batch_document))) == map(batch_document) do doc
                nltk_word_tokenize.(split_sentences(doc))
            end
            @test nestedcall(getmeta, tkr(Batch{Document}(batch_document))) ==
                map(enumerate(split_sentences.(batch_document))) do (i, d)
                    map(enumerate(nltk_word_tokenize.(d))) do (j, s)
                        map(enumerate(s)) do (k, x)
                            (document_id = i, sentence_id = j, word_id = k, token_id = k)
                        end
                    end
                end
        end
    end

    @testset "Vocabulary" begin
        vocab = Vocab(["a", "b", "c", "a", "b", "c"])
        vocab_unk = Vocab(["a", "b", "xxx"], "xxx")
        vocab_char = Vocab{Char}('a':'z', ' ')
        vocab_int = Vocab{Int}(11:20, 0)

        @test length(vocab) == 3
        @test vocab.list == ["a", "b", "c"]
        @test vocab.unki == 0
        @test length(vocab_unk) == 3
        @test vocab_unk.list == ["a", "b", "xxx"]
        @test vocab_unk.unki == 3
        @test vocab_int.list == collect(11:20)
        @test sprint(show, vocab_int) == "Vocab{$Int, StaticArrays.SizedVector{10, $Int, Vector{$Int}}}(size = 10, unk = 0, unki = 0)"

        @testset "lookup" begin
            @test lookup(vocab, "a") == 1
            @test lookup(vocab, "b") == 2
            @test lookup(vocab, "c") == 3
            @test lookup(vocab, "d") == 0

            @test lookup(vocab_unk, "a") == 1
            @test lookup(vocab_unk, "b") == 2
            @test lookup(vocab_unk, "c") == 3
            @test lookup(vocab_unk, "d") == 3

            @test lookup(vocab, 1) == "a"
            @test lookup(vocab, 2) == "b"
            @test lookup(vocab, 3) == "c"
            @test lookup(vocab, 0) == "[UNK]"
            @test lookup(vocab, 1000000) == "[UNK]"

            @test lookup(vocab_unk, 1) == "a"
            @test lookup(vocab_unk, 2) == "b"
            @test lookup(vocab_unk, 3) == "xxx"
            @test lookup(vocab_unk, 100000) == "xxx"

            @test lookup(vocab, 1,2,3,4) == ("a", "b", "c", "[UNK]")
            @test lookup(vocab_unk, 1,2,3,4) == ("a", "b", "xxx", "xxx")

            @test lookup(vocab, [1, "a", 0, "A", "[UNK]"]) == ["a", 1, "[UNK]", 0, 0]
            @test lookup(vocab_unk, [1, "a", 0, "A", "[UNK]"]) == ["a", 1, "xxx", 3, 3]

            @test lookup(vocab, [(1, "a"), (a=0, b="A"), "[UNK]"]) == [("a", 1), (a="[UNK]", b=0), 0]
            @test lookup(vocab_char, ['a', (x='x',)], 26) == ([1, (x=24,)], 'z')
        end

        @testset "lookup int" begin
            @test lookup(vocab_int, 1,2,3) == (11,12,13)
            @test lookup(Int, vocab_int, 1,2,3) == (0,0,0)
            @test lookup(Int, vocab_int, 11,12,13) == (1,2,3)
        end

        @testset "onehot" begin
            @test lookup(OneHot, vocab, "a") == OneHot(3, 1)
            @test lookup(OneHot, vocab, "A") == OneHot(3, 0)
            @test lookup(OneHot, vocab, "a", "b", "c", "d") == OneHotArray(3, [1,2,3,0])
            @test lookup(OneHot, vocab, ["a", "b", "c", "d"]) == OneHotArray(3, [1,2,3,0])
            @test lookup(OneHot, vocab, ["a" "b"; "c" "d"]) == OneHotArray(3, [1 2; 3 0])
            @test lookup(OneHot, vocab, ["a", "b"], ["c", "d"]) == OneHotArray(3, [1,2,3,0])
            @test lookup(OneHot, vocab, ["a"], "b", ["c", "d"], "z") == OneHotArray(3, [1,2,3,0,0])

            @test_throws DomainError lookup(OneHot, vocab, 1)
            @test_throws DomainError lookup(OneHot, vocab, [1,2,3])
            @test_throws DomainError lookup(OneHot, vocab, "1",2,3)
            @test_throws DomainError lookup(OneHot, vocab, ["1",2,3])

            @test lookup(vocab, OneHot(3, 1)) == "a"
            @test lookup(vocab, OneHot(5, 1)) == "a"
            @test lookup(vocab, OneHot(5, 4)) == "[UNK]"
            @test lookup(vocab, OneHotArray(3, [1,2,3,0])) == ["a", "b", "c", "[UNK]"]
            @test lookup(vocab, OneHotArray(3, [1 2 3; 3 0 1])) == ["a" "b" "c"; "c" "[UNK]" "a"]
        end
    end

    @testset "Utils" begin
        @test_inferred nestedcall(x->x+1, [[[[3],[5, 6]]]])

        @testset "with_head_tail" begin
            x = collect(1:5)
            @test with_head_tail(x, 0, 6) == collect(0:6)
            @test with_head_tail(x, nothing, 6) == collect(1:6)
            @test with_head_tail(x, 0, nothing) == collect(0:5)
            @test with_head_tail(x, nothing, nothing) == x
            @test with_head_tail(0, 6)(x) == collect(0:6)
            @test with_head_tail(nothing, 6)(x) == collect(1:6)
            @test with_head_tail(0, nothing)(x) == collect(0:5)
            @test with_head_tail(nothing, nothing)(x) == x

            @test with_head_tail(x; head=0, tail=6) == collect(0:6)
            @test with_head_tail(x, tail=6) == collect(1:6)
            @test with_head_tail(x, head=0) == collect(0:5)
            @test with_head_tail(head=0, tail=6)(x) == collect(0:6)
            @test with_head_tail(tail=6)(x) == collect(1:6)
            @test with_head_tail(head=0)(x) == collect(0:5)

            @test with_head_tail([[x], 1:5, 2:3], -1, -2) == [[[-1;x;-2]], [-1; 1:5; -2], [-1; 2:3; -2]]
        end

        @testset "trunc_and_pad" begin
            x = collect(1:9)
            @test trunc_and_pad(x, 5, 0) == collect(1:5)
            @test trunc_and_pad(1:3, 5, 0) == [1:3; 0; 0]
            @test trunc_and_pad(x, nothing, 0) == collect(1:9)
            @test trunc_and_pad(1:3, nothing, 0) == collect(1:3)

            @test trunc_and_pad(5, 0)(x) == collect(1:5)
            @test trunc_and_pad(5, 0)(1:3) == [1:3; 0; 0]
            @test trunc_and_pad(nothing, 0)(x) == collect(1:9)
            @test trunc_and_pad(nothing, 0)(1:3) == collect(1:3)

            @test trunc_and_pad([[x], 1:5, 2:3], 7, -1) == [[collect(1:7)], [1:5; -1; -1], [2:3; fill(-1, 5)]]
            @test trunc_and_pad([[x], 1:5, 2:3], nothing, -1) == [[collect(1:9)], [1:5; fill(-1, 4)], [2:3; fill(-1, 7)]]
        end

        @testset "nested2batch" begin
            x = randn(5,4,3,2)
            x_slices = [x[i:i+5-1] for i in 1:5:length(x)]
            y = [[[x_slices[1],x_slices[2],x_slices[3],x_slices[4]],
                  [x_slices[5],x_slices[6],x_slices[7],x_slices[8]],
                  [x_slices[9],x_slices[10],x_slices[11],x_slices[12]],],
                 [[x_slices[13],x_slices[14],x_slices[15],x_slices[16]],
                  [x_slices[17],x_slices[18],x_slices[19],x_slices[20]],
                  [x_slices[21],x_slices[22],x_slices[23],x_slices[24]],]]
            y2 = [[cat(x_slices[1],x_slices[2],x_slices[3],x_slices[4], dims=2),
                   cat(x_slices[5],x_slices[6],x_slices[7],x_slices[8], dims=2),
                   cat(x_slices[9],x_slices[10],x_slices[11],x_slices[12], dims=2),],
                  [cat(x_slices[13],x_slices[14],x_slices[15],x_slices[16], dims=2),
                   cat(x_slices[17],x_slices[18],x_slices[19],x_slices[20], dims=2),
                   cat(x_slices[21],x_slices[22],x_slices[23],x_slices[24], dims=2),]]
            y3 = [cat(cat(x_slices[1],x_slices[2],x_slices[3],x_slices[4], dims=2),
                      cat(x_slices[5],x_slices[6],x_slices[7],x_slices[8], dims=2),
                      cat(x_slices[9],x_slices[10],x_slices[11],x_slices[12], dims=2), dims=3),
                  cat(cat(x_slices[13],x_slices[14],x_slices[15],x_slices[16], dims=2),
                      cat(x_slices[17],x_slices[18],x_slices[19],x_slices[20], dims=2),
                      cat(x_slices[21],x_slices[22],x_slices[23],x_slices[24], dims=2), dims=3)]

            @test nested2batch(y) == x
            @test nested2batch(y2) == x
            @test nested2batch(y3) == x

            @test_throws DimensionMismatch nested2batch([[1:5], 2:6])
        end
    end

    @testset "Encoder" begin
        sentence = Sentence("A single sentence with 31 char.")
        tkr = NestedTokenizer(IndexedTokenization(CharTk()))
        vocab = Vocab(map(string, ['a':'z'; 'A':'Z']))
        enc = TextEncoder(tkr, vocab, nested2batch∘nestedcall(getvalue))
        s(x) = mapfoldl(y->split(y,""), append!, split(x); init=String[])
        @test encode(enc, sentence) == reshape(lookup(OneHot, vocab, s(sentence.x)), Val(3))
        @test decode(enc, encode(enc, sentence)) == lookup(vocab, reshape(lookup(OneHot, vocab, s(sentence.x)), Val(3)))

        enc2 = TextEncoder(tkr, vocab) do e
            nested2batch∘TextEncodeBase.process(e)
        end
        @test enc == enc2
    end

    @testset "Pipelines" begin
        p1 = Pipeline{:x}((x,_)->x)
        p2 = Pipeline{(:sinx, :cosx)}((x, _)->sincos(x))
        ps1 = Pipelines(p1, p2)
        ps2 = Pipelines(Pipeline{:x}(identity, 1), Pipeline{(:sinx, :cosx)}(y->sincos(y.x), 2))
        ps3 = ps2 |> PipeGet{:x}()
        ps4 = ps2 |> PipeGet{(:x, :sinx)}()

        @test ps1(0.5) == ps2(0.5)
        @test ps3(0.2) == 0.2
        @test ps4(0.3) == (x = 0.3, sinx = sin(0.3))
        @test_inferred p1(0.3)
        @test_inferred p2(0.5)
        @test_inferred ps1(0.5)
        @test_inferred ps2(0.5)
        @test_inferred ps3(0.5)

        @test p1 |> p2 == ps1
        @test ps1 |> p1 == Pipelines(p1, p2, p1)
        @test p1 |> ps1 == Pipelines(p1, p1, p2)
        @test ps1 |> ps1 == Pipelines(p1, p2, p1, p2)
        @test collect(ps1) == [p1, p2]
        @test_throws Exception Pipeline{:x}(identity, 3)
        @test_throws Exception Pipeline{()}(identity)
        @test_throws Exception Pipelines(())
    end
end

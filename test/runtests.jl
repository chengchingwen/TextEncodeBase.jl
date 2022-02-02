using TextEncodeBase
using Test

using TextEncodeBase: AbstractTokenizer, AbstractTokenization,
    TokenStages, Document, Sentence, Word, Token
using TextEncodeBase: getvalue, getmeta, with_head_tail, trunc_and_pad, nested2batch

using WordTokenizers

const ATR = AbstractTokenizer
const AT = AbstractTokenization

@testset "TextEncodeBase.jl" begin
    @testset "Tokenize" begin
        document = Document("This is the first sentence. And the second one with some number 12345.")
        sentence = Sentence("A single sentence with 31 char.")
        word = Word("word")

        @testset "base tokenizer" begin
            tkr = TextEncodeBase.NaiveTokenizer()
            @test tkr(document) == map(Token, mapfoldl(nltk_word_tokenize, append!, split_sentences(document.x)))
            @test tkr(sentence) == map(Token, nltk_word_tokenize(sentence.x))
            @test tkr(word) == [Token(word.x)]
        end

        @testset "index tokenizer" begin
            tkr = TextEncodeBase.NaiveIndexedTokenizer()
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
            tkr = TextEncodeBase.NaiveMatchTokenizer([r"\d", r"en"])
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
            tkr = TextEncodeBase.NaiveIndexedMatchTokenizer([r"\d", r"en"])
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

        @testset "flat output" begin
            tkr = TextEncodeBase.FlatTokenizer(TextEncodeBase.IndexedTokenization())
            tkr2 = TextEncodeBase.NaiveIndexedTokenizer()

            @test tkr(document) == tkr2(document)
            @test tkr(sentence) == tkr2(sentence)
            @test tkr(word) == tkr2(word)
        end

        @testset "nested output" begin
            # struct NestedTkr <: ATR end
            # TextEncodeBase.tokenization(::NestedTkr) = TextEncodeBase.IndexedTokenization()
            # TextEncodeBase.tokenize(tkr::NestedTkr, t::AT, x::Document) = TextEncodeBase.tokenize_procedure!(push!, Vector[], tkr, t, x)
            # TextEncodeBase.tokenize(tkr::NestedTkr, t::AT, ::Nothing, x::Sentence) = [TextEncodeBase.tokenize_procedure(tkr, t, x)]
            # tkr = NestedTkr()

            tkr = TextEncodeBase.NestedTokenizer(TextEncodeBase.IndexedTokenization())
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
            struct CharTkr <: ATR end
            TextEncodeBase.tokenization(::CharTkr) = TextEncodeBase.IndexedTokenization()
            TextEncodeBase.splitting(::CharTkr, t::AT, x::Word) = split(x.x, "")
            TextEncodeBase.tokenize(tkr::CharTkr, t::AT, x::Word) = TextEncodeBase.tokenize_procedure(tkr, t, x)

            tkr = CharTkr()
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
    end

    @testset "Vocabulary" begin
        vocab = Vocab(["a", "b", "c", "a", "b", "c"])
        vocab_unk = Vocab(["a", "b", "xxx"], "xxx")

        @test length(vocab) == 3
        @test vocab.list == ["a", "b", "c"]
        @test vocab.unki == 0
        @test length(vocab_unk) == 3
        @test vocab_unk.list == ["a", "b", "xxx"]
        @test vocab_unk.unki == 3

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
end

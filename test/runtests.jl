using TextEncodeBase
using Test

using TextEncodeBase: Document, Sentence, Word, Token
using TextEncodeBase: getmeta

using WordTokenizers

@testset "TextEncodeBase.jl" begin

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
        @test map(x->x.x, tkr(document)) == [
            "This", "is", "the", "first", "s",
            "en", "t", "en", "ce", ".", "And",
            "the", "second", "one", "with", "some",
            "number", "1", "2", "3", "4", "5", ".",
        ]
        @test map(x->x.x, tkr(sentence)) == [
            "A", "single", "s", "en", "t", "en",
            "ce", "with", "3", "1", "char", ".",
        ]
        @test map(x->x.x, tkr(word)) == [word.x]
        @test map(x->x.x, tkr(Word("123"))) == ["1", "2", "3"]
    end

    @testset "indexed match tokenizer" begin
        tkr = TextEncodeBase.NaiveIndexedMatchTokenizer([r"\d", r"en"])
        @test map(x->x.x, tkr(document)) == [
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

        @test map(x->x.x, tkr(sentence)) == [
            "A", "single", "s", "en", "t", "en",
            "ce", "with", "3", "1", "char", ".",
        ]
        @test map(getmeta, tkr(sentence)) == map(NamedTuple{(:word_id, :token_id)}, zip(1:12, 1:12))

        @test map(x->x.x, tkr(word)) == [word.x]
        @test map(getmeta, tkr(word)) == [(word_id = 1, token_id = 1)]
        @test map(x->x.x, tkr(Word("123"))) == ["1", "2", "3"]
        @test map(getmeta, tkr(Word("123"))) == map(NamedTuple{(:word_id, :token_id)}, zip(1:3, 1:3))
    end

end

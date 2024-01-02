{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- tests
-}

module Tokenizing (tests) where

import Test.HUnit
import Tokenizer
import Token
import SourcePos

tests :: Test
tests = test
    [ "basicTokenizer" ~: do
        tokenizer "tokenize.hs" "struct object"
            @?= Right [Token "struct" (SourcePos "tokenize.hs" 1 1) StructKw, Token "object" (SourcePos "tokenize.hs" 1 8) TextIdentifier]
        tokenizer "tokenize.hs" "fn two = 2"
            @?= Right [Token "fn" (SourcePos "tokenize.hs" 1 1) FnKw,
                Token "two" (SourcePos "tokenize.hs" 1 4) TextIdentifier,
                Token "=" (SourcePos "tokenize.hs" 1 8) Equal,
                Token "2" (SourcePos "tokenize.hs" 1 10) IntLiter
                ]
        tokenizer "tokenize.hs" "fn two = 2\nfn name = \"Jeremy\""
            @?= Right [Token "fn" (SourcePos "tokenize.hs" 1 1) FnKw,
                Token "two" (SourcePos "tokenize.hs" 1 4) TextIdentifier,
                Token "=" (SourcePos "tokenize.hs" 1 8) Equal,
                Token "2" (SourcePos "tokenize.hs" 1 10) IntLiter,
                Token "fn" (SourcePos "tokenize.hs" 2 1) FnKw,
                Token "name" (SourcePos "tokenize.hs" 2 4) TextIdentifier,
                Token "=" (SourcePos "tokenize.hs" 2 9) Equal,
                Token "Jeremy" (SourcePos "tokenize.hs" 2 11) StringLiter
                ]
        tokenizer "tokenize.hs" "fn array = [1,2,3,4]"
            @?= Right [Token "fn" (SourcePos "tokenize.hs" 1 1) FnKw,
                Token "array" (SourcePos "tokenize.hs" 1 4) TextIdentifier,
                Token "=" (SourcePos "tokenize.hs" 1 10) Equal,
                Token "[" (SourcePos "tokenize.hs" 1 12) BracketOpen,
                Token "1" (SourcePos "tokenize.hs" 1 13) IntLiter,
                Token "," (SourcePos "tokenize.hs" 1 14) Comma,
                Token "2" (SourcePos "tokenize.hs" 1 15) IntLiter,
                Token "," (SourcePos "tokenize.hs" 1 16) Comma,
                Token "3" (SourcePos "tokenize.hs" 1 17) IntLiter,
                Token "," (SourcePos "tokenize.hs" 1 18) Comma,
                Token "4" (SourcePos "tokenize.hs" 1 19) IntLiter,
                Token "]" (SourcePos "tokenize.hs" 1 20) BracketClose
                ]
    , "tokenizerWithComments" ~: do
        tokenizer "comments.hs" "fn two = 2\n//fn name = \"Jeremy\""
            @?= Right [Token "fn" (SourcePos "comments.hs" 1 1) FnKw,
                Token "two" (SourcePos "comments.hs" 1 4) TextIdentifier,
                Token "=" (SourcePos "comments.hs" 1 8) Equal,
                Token "2" (SourcePos "comments.hs" 1 10) IntLiter
                ]
        tokenizer "comments.hs" "fn two = 2\n//fn name = \"Jeremy\"\nfn name = \"Jeremy\""
            @?= Right [Token "fn" (SourcePos "comments.hs" 1 1) FnKw,
                Token "two" (SourcePos "comments.hs" 1 4) TextIdentifier,
                Token "=" (SourcePos "comments.hs" 1 8) Equal,
                Token "2" (SourcePos "comments.hs" 1 10) IntLiter,
                Token "fn" (SourcePos "comments.hs" 3 1) FnKw,
                Token "name" (SourcePos "comments.hs" 3 4) TextIdentifier,
                Token "=" (SourcePos "comments.hs" 3 9) Equal,
                Token "Jeremy" (SourcePos "comments.hs" 3 11) StringLiter
                ]
        tokenizer "comments.hs" "/* fn two = 2\nfn name = \"Jeremy\" */\nfn name = \"Jeremy\""
            @?= Right [Token "fn" (SourcePos "comments.hs" 3 1) FnKw,
                Token "name" (SourcePos "comments.hs" 3 4) TextIdentifier,
                Token "=" (SourcePos "comments.hs" 3 9) Equal,
                Token "Jeremy" (SourcePos "comments.hs" 3 11) StringLiter
                ]

    , "tokenizerOperators" ~: do
        tokenizer "operators.hs" "fn two = 2\nfn name = \"Jeremy\"\nfn name = \"Jeremy\" + 2"
            @?= Right [Token "fn" (SourcePos "operators.hs" 1 1) FnKw,
                Token "two" (SourcePos "operators.hs" 1 4) TextIdentifier,
                Token "=" (SourcePos "operators.hs" 1 8) Equal,
                Token "2" (SourcePos "operators.hs" 1 10) IntLiter,
                Token "fn" (SourcePos "operators.hs" 2 1) FnKw,
                Token "name" (SourcePos "operators.hs" 2 4) TextIdentifier,
                Token "=" (SourcePos "operators.hs" 2 9) Equal,
                Token "Jeremy" (SourcePos "operators.hs" 2 11) StringLiter,
                Token "fn" (SourcePos "operators.hs" 3 1) FnKw,
                Token "name" (SourcePos "operators.hs" 3 4) TextIdentifier,
                Token "=" (SourcePos "operators.hs" 3 9) Equal,
                Token "Jeremy" (SourcePos "operators.hs" 3 11) StringLiter,
                Token "+" (SourcePos "operators.hs" 3 20) OperatorIdentifier,
                Token "2" (SourcePos "operators.hs" 3 22) IntLiter
                ]

        tokenizer "operators.hs" "fn calcul = 2 * 2 / 4"
            @?= Right [Token "fn" (SourcePos "operators.hs" 1 1) FnKw,
                Token "calcul" (SourcePos "operators.hs" 1 4) TextIdentifier,
                Token "=" (SourcePos "operators.hs" 1 11) Equal,
                Token "2" (SourcePos "operators.hs" 1 13) IntLiter,
                Token "*" (SourcePos "operators.hs" 1 15) OperatorIdentifier,
                Token "2" (SourcePos "operators.hs" 1 17) IntLiter,
                Token "/" (SourcePos "operators.hs" 1 19) OperatorIdentifier,
                Token "4" (SourcePos "operators.hs" 1 21) IntLiter
                ]
        tokenizer "operators.hs" "fn calcul = 3 | 3 & 1 ^ 2"
            @?= Right [Token "fn" (SourcePos "operators.hs" 1 1) FnKw,
                Token "calcul" (SourcePos "operators.hs" 1 4) TextIdentifier,
                Token "=" (SourcePos "operators.hs" 1 11) Equal,
                Token "3" (SourcePos "operators.hs" 1 13) IntLiter,
                Token "|" (SourcePos "operators.hs" 1 15) Pipe,
                Token "3" (SourcePos "operators.hs" 1 17) IntLiter,
                Token "&" (SourcePos "operators.hs" 1 19) OperatorIdentifier,
                Token "1" (SourcePos "operators.hs" 1 21) IntLiter,
                Token "^" (SourcePos "operators.hs" 1 23) OperatorIdentifier,
                Token "2" (SourcePos "operators.hs" 1 25) IntLiter
                ]

        tokenizer "operators.hs" "fn calcul = 3 % 2"
            @?= Right [Token "fn" (SourcePos "operators.hs" 1 1) FnKw,
                Token "calcul" (SourcePos "operators.hs" 1 4) TextIdentifier,
                Token "=" (SourcePos "operators.hs" 1 11) Equal,
                Token "3" (SourcePos "operators.hs" 1 13) IntLiter,
                Token "%" (SourcePos "operators.hs" 1 15) OperatorIdentifier,
                Token "2" (SourcePos "operators.hs" 1 17) IntLiter
                ]

        tokenizer "operators.hs" "fn calcul = 1..4 + 2"
            @?= Right [Token "fn" (SourcePos "operators.hs" 1 1) FnKw,
                Token "calcul" (SourcePos "operators.hs" 1 4) TextIdentifier,
                Token "=" (SourcePos "operators.hs" 1 11) Equal,
                Token "1" (SourcePos "operators.hs" 1 13) IntLiter,
                Token ".." (SourcePos "operators.hs" 1 14) DotDot,
                Token "4" (SourcePos "operators.hs" 1 16) IntLiter,
                Token "+" (SourcePos "operators.hs" 1 18) OperatorIdentifier,
                Token "2" (SourcePos "operators.hs" 1 20) IntLiter
                ]

    , "keywordTokenizer" ~: do
        tokenizer "kw.hs" "struct object {\n\tname: string\n}"
            @?= Right [Token "struct" (SourcePos "kw.hs" 1 1) StructKw,
                Token "object" (SourcePos "kw.hs" 1 8) TextIdentifier,
                Token "{" (SourcePos "kw.hs" 1 15) CurlyOpen,
                Token "name" (SourcePos "kw.hs" 2 2) TextIdentifier,
                Token ":" (SourcePos "kw.hs" 2 6) Colon,
                Token "string" (SourcePos "kw.hs" 2 8) TextIdentifier,
                Token "}" (SourcePos "kw.hs" 3 1) CurlyClose
                ]
        tokenizer "kw.hs" "atom true"
            @?= Right [Token "atom" (SourcePos "kw.hs" 1 1) AtomKw,
                Token "true" (SourcePos "kw.hs" 1 6) TextIdentifier
                ]

        tokenizer "kw.hs" "type bool = true | false"
            @?= Right [Token "type" (SourcePos "kw.hs" 1 1) TypeKw,
                Token "bool" (SourcePos "kw.hs" 1 6) TextIdentifier,
                Token "=" (SourcePos "kw.hs" 1 11) Equal,
                Token "true" (SourcePos "kw.hs" 1 13) TextIdentifier,
                Token "|" (SourcePos "kw.hs" 1 18) Pipe,
                Token "false" (SourcePos "kw.hs" 1 20) TextIdentifier
                ]
    ]

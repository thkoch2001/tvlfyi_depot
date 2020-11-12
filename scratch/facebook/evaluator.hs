module Evaluator where

data Token
  = TokenInt Integer
  | TokenAdd
  | TokenMultiply
  deriving (Eq, Show)

newtype AST = AST [Token]
  deriving (Eq, Show)

tokens :: [Token]
tokens =
  [ TokenInt 13
  , TokenAdd
  , TokenInt 2
  , TokenMultiply
  , TokenInt 4
  , TokenAdd
  , TokenInt 7
  , TokenAdd
  , TokenInt 3
  , TokenMultiply
  , TokenInt 8
  ]

-- expression     -> addition ;
-- addition       -> multiplication ( "+" multiplication )* ;
-- multiplication -> terminal ( "*" terminal )* ;
-- terminal       -> NUMBER ;

parseExpression :: [Token] -> ([Token], AST)
parseExpression tokens = do
  lhs, rest = parseMultiplication tokens

parseMulitplication :: [Token] -> ([Token], AST)

main :: IO ()
main = print $ parse tokens

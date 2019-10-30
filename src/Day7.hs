module Day7 where

import Text.Read
import Text.Trifecta

data OpCode = 
      Not Operand
    | Or Operand Operand
    | And Operand Operand
    | Rshift Operand Integer
    | Lshift Operand Integer
    | Value Integer
    deriving (Eq, Show)

data Node = Node ID OpCode 

type ID = String

data Operand =
      Literal Integer
    | Identifier ID
    deriving (Eq, Show)

convertInt :: String -> Maybe Integer
convertInt = readMaybe

parseNot :: Parser Node
parseNot = try $ do
    symbol "NOT"
    ident1 <- noneOf " "
    symbol " -> "
    ident2 <- noneOf " "
    pure $ Node ident2 $ Not )

    

parse :: Parser OpCode
parse = undefined
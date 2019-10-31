module Day7 where

import Control.Applicative
import Text.Trifecta
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust)

data OpCode = 
      Not Operand
    | Or Operand Operand
    | And Operand Operand
    | Rshift Operand Operand
    | Lshift Operand Operand
    | Value Integer
    deriving (Eq, Show)

data Node = Node ID OpCode deriving (Eq, Show)

type ID = String

data Operand =
      Literal Integer
    | Identifier ID
    deriving (Eq, Show)

nodeMap :: [Node] -> Map ID Node
nodeMap xs = M.fromList $ zip (getID <$> xs) xs
    where getID (Node i _ ) = i

parseOperand :: Parser Operand
parseOperand = 
        Literal . read <$> some digit 
    <|> Identifier <$> some letter

parseNot :: Parser OpCode
parseNot = try $ Not <$> (string "NOT" *> whiteSpace *> token parseOperand)

-- This parseOperand has a side effect! I'm just applying a function to it
-- but this is what is meant by an effect or side effect. It's technically pure
-- but the effect is outside of the immediate obvious scope
parseInfix :: (Operand -> Operand -> OpCode) -> String -> Parser OpCode
parseInfix con op = try $ mk <$> parseOperand <*> pad op <*> token parseOperand
    where mk o1 _ o2 = con o1 o2
          pad s = string s `surroundedBy` whiteSpace
        
parseOr = parseInfix Or "OR"
parseLs = parseInfix Lshift "LSHIFT"
parseRs = parseInfix Rshift "RSHIFT"
parseAnd = parseInfix And "AND"

parseValue :: Parser OpCode
parseValue = Value . read <$> token (some digit)

parseNode :: Parser Node
parseNode = 
    mkNode <$> choice [parseOr, parseLs, parseRs, parseAnd, parseNot, parseValue]
           <*> string "-> " <*> some letter
    where mkNode v _ i = Node i v

parse :: Parser (Map ID Node)
parse = nodeMap <$> many (token parseNode)

day7 :: IO ()
day7 = parseFromFile parse "input/day7.txt" >>= print . fromJust
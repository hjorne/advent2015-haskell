module Day7 where

import Control.Applicative
import Text.Trifecta
import Data.Map (Map, (!))
import Data.Bits
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Debug.Trace

data OpCode = 
      Not Operand
    | Or Operand Operand
    | And Operand Operand
    | Rshift Operand Operand
    | Lshift Operand Operand
    | Value Operand
    deriving (Eq, Show)

data Node = Node ID OpCode deriving (Eq, Show)

type Graph = Map ID Node

type ID = String

data Operand =
      Literal Integer
    | Identifier ID
    deriving (Eq, Show)

-- I accidentally stumbled on mutual recursion?
-- I guess a natural occurrence from ASTs?
evalNode :: Graph -> ID -> Integer
evalNode g i = g' ! i
    where g' = fmap (evalOp g) g

evalOpCode :: Graph -> Node -> Integer
evalOpCode g (Node i op) = 
    case op of 
        Not op -> complement $ evalNode g i
        Or op1 op2 -> evalOpCodeerand g op1 .|. evalOpCodeerand g op2
        And op1 op2 -> evalOpCodeerand g op1 .&. evalOpCodeerand g op2
        Rshift op1 op2 -> evalOpCodeerand g op1 `shiftR` fromInteger (evalOpCodeerand g op2)
        Lshift op1 op2 -> evalOpCodeerand g op1 `shiftL` fromInteger (evalOpCodeerand g op2)
        Value i -> evalOpCodeerand g i

evalOperand :: Graph -> Operand -> Integer
evalOperand 

-- -- I accidentally stumbled on mutual recursion?
-- -- I guess a natural occurrence from ASTs?
-- evalOpCode :: Graph -> OpCode -> Integer
-- evalOpCode g (Not op) = complement $ evalOperand g op
-- evalOpCode g (Or op1 op2) = evalOperand g op1 .|. evalOperand g op2
-- evalOpCode g (And op1 op2) = evalOperand g op1 .&. evalOperand g op2
-- evalOpCode g (Rshift op1 op2) = evalOperand g op1 `shiftR` fromInteger (evalOperand g op2)
-- evalOpCode g (Lshift op1 op2) = evalOperand g op1 `shiftL` fromInteger (evalOperand g op2)
-- evalOpCode g (Value i) = evalOperand g i


-- evalOperand :: Graph -> Operand -> Integer
-- evalOperand _ (Literal i) = i
-- evalOperand g (Identifier i) = evalOpCode g' op
--     where Node key op = g ! i
--           ret = evalOpCode g op
--           newNode = Node key (Value $ Literal ret)
--           g' = M.insert key newNode g


nodeMap :: [Node] -> Graph
nodeMap xs = M.fromList $ zip (getID <$> xs) xs
    where getID (Node i _ ) = i

-- This parseOperand has a side effect! I'm just applying a function to it
-- but this is what is meant by an effect or side effect. It's technically pure
-- but the effect is outside of the immediate obvious scope
parseInfix :: (Operand -> Operand -> OpCode) -> String -> Parser OpCode
parseInfix con op = try $ mk <$> parseOperand <*> pad op <*> token parseOperand
    where mk o1 _ o2 = con o1 o2
          pad s = string s `surroundedBy` whiteSpace
        
parseOperand = 
        Literal . read <$> some digit 
    <|> Identifier <$> some letter
parseNot = try $ Not <$> (string "NOT" *> whiteSpace *> token parseOperand)
parseOr = parseInfix Or "OR"
parseLs = parseInfix Lshift "LSHIFT"
parseRs = parseInfix Rshift "RSHIFT"
parseAnd = parseInfix And "AND"
parseValue = Value <$> token parseOperand
parseNode = 
    mkNode <$> choice [parseOr, parseLs, parseRs, parseAnd, parseNot, parseValue]
           <*> string "-> " <*> some letter
    where mkNode v _ i = Node i v

parse :: Parser Graph
parse = nodeMap <$> many (token parseNode)

day7 :: IO ()
day7 = do 
    g <- parseFromFile parse "input/day7.txt" 
    let (Node _ op) = fromJust g ! "a"
    return ()
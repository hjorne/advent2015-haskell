module Day7 where

import Control.Applicative
import Text.Trifecta
import Data.Map (Map, (!))
import Data.Set (Set)
import qualified Data.Set as S
import Data.Bits
import Data.Foldable (foldl')
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

data Node = Node ID [ID] OpCode deriving (Eq, Show)
data Tree a = Tree a [Tree a] deriving (Eq, Show)

type Graph = Map ID Node

type ID = String

data Operand =
      Literal Integer
    | Identifier ID
    deriving (Eq, Show)

generate :: Graph -> ID -> Tree ID
generate g v = Tree v (generate g <$> neighbors)
    where Node _ neighbors _ = g ! v

chop :: Tree ID -> Tree ID
chop t = head . snd $ chopF S.empty [t]

chopF :: Set ID -> [Tree ID] -> (Set ID, [Tree ID])
chopF visited [] = (visited, [])
-- A kind of backtracking? Probably pretty similar to others of its nature
chopF visited (Tree v _:us) | S.member v visited = chopF visited us
chopF visited (Tree v ts:us) = (v''', newT:chopright)
    where v' = S.insert v visited
          (v'', chopleft) = chopF v' ts
          (v''', chopright) = chopF v'' us
          newT = Tree v chopleft

dfs :: Graph -> ID -> Tree ID
dfs g v = chop $ generate g v

postorder :: Tree ID -> [ID]
postorder (Tree v []) = [v]
postorder (Tree v xs) = (xs >>= postorder) ++ [v]

nodeMap :: [Node] -> Graph
nodeMap xs = M.fromList $ zip (getID <$> xs) xs
    where getID (Node i _ _ ) = i

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
    where mkNode v _ i = Node i (genNeighbors v) v

genNeighbors :: OpCode -> [ID]
genNeighbors (Not op) = operandToId op
genNeighbors (Or op1 op2) = operandToId op1 ++ operandToId op2 
genNeighbors (And op1 op2) = operandToId op1 ++ operandToId op2 
genNeighbors (Rshift op1 op2) = operandToId op1 ++ operandToId op2 
genNeighbors (Lshift op1 op2) = operandToId op1 ++ operandToId op2 
genNeighbors (Value op) = operandToId op

operandToId :: Operand -> [ID]
operandToId (Identifier i) = [i]
operandToId (Literal _) = []

parse :: Parser Graph
parse = nodeMap <$> many (token parseNode)

day7 :: IO ()
day7 = do 
    g <- parseFromFile parse "input/day7.txt" 
    let g' = fromJust g
    print $ postorder $ dfs g' "a"
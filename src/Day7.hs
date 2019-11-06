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

type Graph = Map ID Node

type ID = String

data Operand =
      Literal Integer
    | Identifier ID
    deriving (Eq, Show)


-- dfs :: Graph -> Set ID -> [ID] -> ID -> [ID]
-- dfs _ visited tsort node | S.member node visited = tsort
-- dfs g visited tsort node | trace ("Visited: " ++ show visited ++ " node: " ++ show node ++ " tsort: " ++ show tsort) S.member node visited = tsort
-- dfs g visited tsort node = neighbors >>= \n -> dfs g visited' (n:tsort) n
--     where (Node _ neighbors _) = g ! node
--           visited' = S.insert node visited

dfs :: Graph -> Set ID -> [ID] -> ID -> [ID]
dfs g visited tsort node | trace ("Visited: " ++ show visited ++ " node: " ++ show node ++ " tsort: " ++ show tsort ++ "\n") False = undefined
dfs _ visited _ node | S.member node visited = [node]
dfs g _ _ node | trace ("dfs neighbors " ++ show neighbors) False = undefined
    where (Node _ neighbors _) = g ! node

dfs g visited tsort node = 
    case neighbors of 
        [] -> [node]
        _  -> snd $ foldl' go (visited, tsort) neighbors
    where Node _ neighbors _ = g ! node
          go _ node | trace ("go node " ++ show node) False = undefined
          go (visited, tsort) node = (visited', dfs g visited' [] node ++ tsort)
            where visited' = S.insert node visited


nodeMap :: [Node] -> Graph
nodeMap xs = M.fromList $ zip (getID <$> xs) xs
    where getID (Node i _ _ ) = i

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
    print $ M.elems $ fromJust g
    -- print $ dfs (fromJust g) S.empty ["q"] "q"
    return ()
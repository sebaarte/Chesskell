{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module AI(getNextMove) where
import Data.Maybe(fromJust)
import Data.List(elemIndex)
import Debug.Trace(trace)

import Board
import Case
import Player
import ChessGameData
import Move
import MoveTree
import MovementHandling

-- used by the fitness function
pieceValue:: Case -> Int
pieceValue (Case White Pawn) = -1
pieceValue (Case White Knight) = -3 
pieceValue (Case White Bishop) = -3 
pieceValue (Case White Rook) = -5
pieceValue (Case White Queen) = -9
pieceValue (Case Black Pawn) = 1
pieceValue (Case Black Knight) = 3 
pieceValue (Case Black Bishop) = 3 
pieceValue (Case Black Rook) = 5
pieceValue (Case Black Queen) = 9
pieceValue _ = 0


-- depending on the current player and the AI player, return either the maximum or minimum of the list. In case the list is empty, zero is returned (endgame)
customMaximum:: [Int] -> Player -> Player -> Int
customMaximum [] _ _ = 0
customMaximum xs player currPlayer = if player == currPlayer then maximum xs else minimum xs

-- helper function for the fitness function that evaluates only a row
rowFitness:: [Case] -> Int
rowFitness x = sum (map (pieceValue) x)

-- evaluate a board situation  for AI
fitnessFunction:: ChessGameState-> Player -> Int
fitnessFunction st@(ChessGameState board _ _) aiPlayer = if isWinner aiPlayer st
                                                then 100000000  -- arbitrarily big number
                                                else 
                                                    if isWinner (otherPlayer aiPlayer) st
                                                    then -100000000
                                                    else sum (map (rowFitness) board)

-- query the AI to get the next move
getNextMove:: ChessGameState -> Player -> IO Command
getNextMove state aiPlayer = putStr "Ai playing...\n" >> putStr ("AI played: " ++ move ++ "\n") >> return move where move = (fromJust (toString (extractBestMove state aiPlayer)))

-- choose the best move among all combinations
-- TODO make the selection random among best candidates
extractBestMove:: ChessGameState -> Player -> Move
extractBestMove st@ChessGameState{board,turn,moveHistory} aiPlayer =    let (Level _ subtrees) = constructTree (Root st) 2
                                                                            subtreeValues = map (getTreeValue aiPlayer) subtrees
                                                                            bestIdx = fromJust (elemIndex (customMaximum subtreeValues turn aiPlayer) subtreeValues)
                                                                            res = getLastMove (subtrees !! bestIdx)
                                                                        in  res


-- compute fitness value of a tree
getTreeValue:: Player -> MoveTree -> Int
getTreeValue aiPlayer (Leaf st) = fitnessFunction st aiPlayer
getTreeValue aiPlayer (Level st@ChessGameState{board,turn,..} leaves) = customMaximum (map (getTreeValue aiPlayer) leaves) turn aiPlayer
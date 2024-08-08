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

customMaximum:: [Int] -> Int -> Player -> Player -> Int
customMaximum [] def _ _ = def
customMaximum xs _ player currPlayer = if player == currPlayer then maximum xs else minimum xs


rowFitness:: [Case] -> Int
rowFitness x = sum (map (pieceValue) x)

fitnessFunction:: ChessGameState-> Int
fitnessFunction st@(ChessGameState board _ _) = if isWinner Black st
                                                then 100000000  -- arbitrarily big number
                                                else 
                                                    if isWinner White st
                                                    then -100000000
                                                    else sum (map (rowFitness) board)

getNextMove:: ChessGameState -> Player -> IO Command
getNextMove state aiPlayer = putStr "Ai playing...\n" >> putStr ("AI played: " ++ move ++ "\n") >> return move where move = (fromJust (toString (extractBestMove state aiPlayer)))


extractBestMove:: ChessGameState -> Player -> Move
extractBestMove st@ChessGameState{board,turn,moveHistory} aiPlayer =    let (Level _ subtrees) = constructTree (Root st) 2
                                                                            subtreeValues = map (getTreeValue aiPlayer) subtrees
                                                                            bestIdx = fromJust (elemIndex (customMaximum subtreeValues 0 turn aiPlayer) subtreeValues)
                                                                            res = getLastMove (subtrees !! bestIdx)
                                                                        in  res



getTreeValue:: Player -> MoveTree -> Int
getTreeValue aiPlayer (Leaf st) = fitnessFunction st
getTreeValue aiPlayer (Level st@ChessGameState{board,turn,..} leaves) = customMaximum (map (getTreeValue aiPlayer) leaves) (fitnessFunction st) turn aiPlayer
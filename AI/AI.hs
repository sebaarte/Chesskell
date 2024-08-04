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

customMaximum:: [Int] ->Int -> Player -> Int
customMaximum [] def _ = def
customMaximum xs _ Black = maximum xs
customMaximum xs _ White = minimum xs

rowFitness:: [Case] -> Int
rowFitness x = sum (map (pieceValue) x)

fitnessFunction:: ChessGameState-> Int
fitnessFunction st@(ChessGameState (Board board) _ _) = if isWinner Black st
                                                then 100000000  -- arbitrarily big number
                                                else 
                                                    if isWinner White st
                                                    then -100000000
                                                    else sum (map (rowFitness) board)

getNextMove:: ChessGameState -> IO Command
getNextMove state = putStr "Ai playing...\n" >> putStr ("AI played: " ++ move ++ "\n") >> return move where move = (fromJust (toString (extractBestMove state)))


extractBestMove:: ChessGameState -> Move
extractBestMove st@ChessGameState{board,turn,moveHistory} = let (Level _ subtrees) = constructTree (Root st) 3
                                                                subtreeValues = map (getTreeValue) subtrees
                                                                bestIdx = fromJust (elemIndex (customMaximum subtreeValues 0 turn) subtreeValues)
                                                                res = getLastMove (subtrees !! bestIdx)
                                                            in  res



getTreeValue:: MoveTree -> Int
getTreeValue (Leaf st) = fitnessFunction st
getTreeValue (Level st@ChessGameState{board,turn,..} leaves) = customMaximum (map getTreeValue leaves) (fitnessFunction st) turn
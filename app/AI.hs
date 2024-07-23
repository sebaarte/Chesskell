{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module AI(getNextMove) where
import Board
import Case
import Player
import ChessGameData
import Move

pieceValue:: Case -> Int
pieceValue (Case One Pawn) = 1
pieceValue (Case One Knight) = 3 
pieceValue (Case One Bishop) = 3 
pieceValue (Case One Rook) = 5
pieceValue (Case One Queen) = 9
pieceValue (Case Two Pawn) = -1
pieceValue (Case Two Knight) = -3 
pieceValue (Case Two Bishop) = -3 
pieceValue (Case Two Rook) = -5
pieceValue (Case Two Queen) = -9
pieceValue _ = 0

fitnessFunction:: [Case] -> Int
fitnessFunction (x:xs) = pieceValue x + fitnessFunction xs


getNextMove:: ChessGameState -> IO Command
getNextMove state = putStr "Ai playing...\n" >> return ("a7a6")
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main (ErrorMsg,main) where

import Data.Char (isAlphaNum)

import Control.Monad (unless)
import Text.Read (readEither)
import System.Random (mkStdGen, randomR)
import GHC.IO.Handle (hFlush)
import GHC.IO.Handle.FD (stdout)
import Text.ParserCombinators.ReadP (string)
import Control.Applicative (Alternative(empty))
import Data.Either (isLeft,isRight)
import Data.Either.Utils(fromRight)


import Pos
import MovementHandling
import Player
import Board
import Case
import ChessGameData
import AI


-- user input for a simple terminal-based game is just a single-line string

type ErrorMsg = String 

promptForInput :: IO Command
promptForInput = putStr "> " >> hFlush stdout >> fmap (filter isAlphaNum) getLine

determineInput:: ChessGameState -> IO Command
determineInput st@ChessGameState{board,turn}
    | turn == One = promptForInput
    | otherwise = getNextMove st

-- We use a type s to represent a game state, where ...
-- ... nextState computes the next game state, given the current state and next user input (may fail on invalid input)
-- ... isFinalState checks whether a given state is a final state 
class GameState s where
    nextState :: s -> Command -> Either ErrorMsg s
    isFinalState :: s -> Bool

-- To "boot" a terminal-based game, we use a type s to represent game state and a type c to represent game configuration, where ...
-- ... we can compute an initial game state s using a given configuration c (which can fail if the configuration is invalid)
class GameState s => TerminalGame s c | c -> s where
    initialState :: c -> Either ErrorMsg s

-- run a game in the terminal
--runGame :: (Show s, TerminalGame s c) => c -> IO ()
runGame = either error loop . initialState
    where loop st = do print st
                       unless (isFinalState st) $ do
                            let tmp = determineInput st
                            cmd <- tmp 
                            let nxt = nextState st cmd
                            either ((>> loop st) . putStrLn) loop nxt


---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Chess Game
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

noMove = (Move (Pos 0 0) (Pos 0 0))


instance GameState ChessGameState where
    nextState state@ChessGameState{board,turn} input
        | isLeft move =  Left "Unable to parse move"
        | isValidMove state  (fromRight move) = Right (ChessGameState (applyMove board (fromRight move)) (passTurn turn) (fromRight move))
        | otherwise = Left "Invalid Move provided"
            where move = fromString input
    isFinalState state = isCheckmate state || isDraw state
               
    
instance TerminalGame ChessGameState ChessGameConfig where
    initialState ChessGameConfig{..} = Right (ChessGameState initialBoard One noMove)
    



main = runGame (ChessGameConfig True True)
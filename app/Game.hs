{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}



module Game (nextState,isFinalState,initialState,defaultInitialState) where
import Data.Either (isLeft,isRight)
import Data.Either.Utils(fromRight)
import Data.Char (isAlphaNum)
import Data.Maybe(fromJust,isJust)
import Debug.Trace(trace)
import Control.Monad (unless,when)
import Text.Read (readEither)
import System.Random (mkStdGen, randomR)
import GHC.IO.Handle (hFlush)
import GHC.IO.Handle.FD (stdout)
import Text.ParserCombinators.ReadP (string)
import Control.Applicative (Alternative(empty))
import System.TimeIt
import System.Exit
import System.Environment   
import Data.List
import System.Directory
import Graphics.Gloss

import Move
import ChessGameData
import Board
import Player
import MoveHistory
import MovementHandling
import GrammarReader
import Pos
import Token


type ErrorMsg = String

-- We use a type s to represent a game state, where ...
-- ... nextState computes the next game state, given the current state and next user input (may fail on invalid input)
-- ... isFinalState checks whether a given state is a final state 
class GameState s where
    nextState :: s -> Move -> s
    isFinalState :: s -> Bool

-- To "boot" a terminal-based game, we use a type s to represent game state and a type c to represent game configuration, where ...
-- ... we can compute an initial game state s using a given configuration c (which can fail if the configuration is invalid)
class GameState s => TerminalGame s c | c -> s where
    initialState :: c -> IO (Either ErrorMsg s)

-- initial board for a game of chess
initialBoard = [playerRow White,pawnRow White] ++ replicate 4 emptyRow ++ [pawnRow Black,playerRow Black]

-- initial state for a game of chess
defaultInitialState = (ChessGameState initialBoard White (EntryHistory [] ))

instance GameState ChessGameState where
    nextState state@ChessGameState{board,turn,moveHistory} move@(Move from to)
--        | input == "undo" = if canUndo moveHistory then Right (undoLastMove state) else Left "Impossible to undo: no previous move"
        | isValidMove state move = (ChessGameState (applyMove board move) (otherPlayer turn) (appendMove moveHistory board move))
        | otherwise = trace ("[Game.hs]: invalid move from= "++ toString from ++ " to= " ++ toString to) state
    isFinalState state = isCheckmate state || isDraw state
               
    
instance TerminalGame ChessGameState ChessGameConfig where
    initialState ChessGameConfig{..} = do
                                        fileExists <- doesFileExist fileName
                                        if fileExists then parseChessFile fileName else return (Right defaultInitialState)
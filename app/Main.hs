{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main (ErrorMsg,main,initialState,runGame) where

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
import Data.Either (isLeft,isRight)
import Data.Either.Utils(fromRight)
import System.TimeIt
import System.Exit
import System.Environment   
import Data.List
import System.Directory

import Pos
import MovementHandling
import Player
import Board
import Case
import ChessGameData
import AI
import MoveHistory
import GrammarReader
import Move


-- user input for a simple terminal-based game is just a single-line string

type ErrorMsg = String 

promptForInput :: IO Command
promptForInput = putStr "> " >> hFlush stdout >> fmap (filter isAlphaNum) getLine

-- either ask the user for input or queries AI for the next move 
determineInput:: ChessGameState -> Player -> Bool -> IO Command
determineInput st@ChessGameState{board,turn} human pvp
    | pvp || turn == human = promptForInput
    | otherwise = getNextMove st (otherPlayer human)

-- test board for debugging and testing purposes
testBoard:: Board
testBoard = multipleReplace b [((Pos 0 0),(Case Black King)),((Pos 7 0),(Case White King)),((Pos 5 4),(Case White Pawn)),((Pos 6 4),(Case Black Pawn))]
                where b = emptyBoard

-- We use a type s to represent a game state, where ...
-- ... nextState computes the next game state, given the current state and next user input (may fail on invalid input)
-- ... isFinalState checks whether a given state is a final state 
class GameState s where
    nextState :: s -> Command -> Either ErrorMsg s
    isFinalState :: s -> Bool

-- To "boot" a terminal-based game, we use a type s to represent game state and a type c to represent game configuration, where ...
-- ... we can compute an initial game state s using a given configuration c (which can fail if the configuration is invalid)
class GameState s => TerminalGame s c | c -> s where
    initialState :: c -> IO (Either ErrorMsg s)

-- run a game in the terminal
--runGame :: (Show s, TerminalGame s c) => c -> IO ()
runGame c =  initialState c >>= either putStrLn loop
    where loop st = do 
                       print st
                       unless (isFinalState st) $ do   
                            let tmp = determineInput st (if playWhite c then White else Black) (pvp c)
                            cmd <- tmp 
                            when (cmd == "stop")  (saveFile st (fileName c) >> exitSuccess)  
                            let nxt = nextState st cmd
                            either ((>> loop st) . putStrLn) loop nxt


---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Chess Game
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------




instance GameState ChessGameState where
    nextState state@ChessGameState{board,turn,moveHistory} input
        | isLeft move =  Left "Unable to parse move"
        | isValidMove state  (fromRight move) = Right (ChessGameState (applyMove board (fromRight move)) (otherPlayer turn) (appendMove moveHistory board (fromRight move)))
        | otherwise = Left "Invalid Move provided"
            where move = (fromStr input)
    isFinalState state = isCheckmate state || isDraw state
               
    
instance TerminalGame ChessGameState ChessGameConfig where
    initialState ChessGameConfig{..} = do
                                        fileExists <- doesFileExist fileName
                                        if fileExists then parseChessFile fileName else return (Right defaultInitialState)


    

main = do
        args <- getArgs
        if length args < 3 then error "Program needs at least two arguments"
        else  runGame (ChessGameConfig ((args !! 0) == "pvp") ((args !! 1) == "w") (args !! 2))
        
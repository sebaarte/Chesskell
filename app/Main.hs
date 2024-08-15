{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main (ErrorMsg,main) where
import Data.Either (isLeft,isRight)
import Data.Either.Utils(fromRight)
import System.TimeIt
import System.Exit
import System.Environment   
import Data.List
import System.Directory
import Graphics.Gloss

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
import SaveFile
import Undo
import MainWindow
import GUIdata
import InputHandler
import Render
import Game


-- user input for a simple terminal-based game is just a single-line string

type ErrorMsg = String 

-- promptForInput :: IO Command
-- promptForInput = putStr "> " >> hFlush stdout >> fmap (filter isAlphaNum) getLine

-- -- either ask the user for input or queries AI for the next move 
-- determineInput:: ChessGameState -> Player -> Bool -> IO Command
-- determineInput st@ChessGameState{board,turn} human pvp
--     | pvp || turn == human = promptForInput
--     | otherwise = getNextMove st (otherPlayer human)





-- -- run a game in the terminal
-- --runGame :: (Show s, TerminalGame s c) => c -> IO ()
-- runGame c =  initialState c >>= either putStrLn loop
--     where loop st = do 
--                         print st
--                         unless (isFinalState st) $ do   
--                                 let tmp = determineInput st (if playWhite c then White else Black) (pvp c)
--                                 cmd <- tmp 
--                                 when (cmd == "stop")  (saveFile st (fileName c) >> exitSuccess)  -- this is not put elsewhere as it stops the "normal flow" of the program
--                                 let nxt = nextState st cmd
--                                 either ((>> loop st) . putStrLn) loop nxt


---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Chess Game
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------





main = do
        args <- getArgs
        let fileName = if length args < 1 then "" else (args !! 0)
        let config = ChessGameConfig True True fileName
        tmp <- (initialState config)
        let init = if isLeft tmp then error "Unable to parse file" else GUIstate (fromRight tmp) Nothing
        play mainWindow black 1 (init) render inputHandler (\f st -> st)
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}



module ChessGameData (ChessGameConfig(..),ChessGameState(..),initialBoard,Command,defaultInitialState) where
import Board
import Move
import Case
import Player
import Board
import MoveHistory

type Command = String

data ChessGameConfig = ChessGameConfig {pvp::Bool, playWhite::Bool,fileName::FilePath}
data ChessGameState = ChessGameState{board::Board,turn::Player,moveHistory::MoveHistory}

-- initial board for a game of chess
initialBoard = [playerRow White,pawnRow White] ++ replicate 4 emptyRow ++ [pawnRow Black,playerRow Black]

-- initial state for a game of chess
defaultInitialState = (ChessGameState initialBoard White (EntryHistory [] ))






        
-- converts a chess state to a string
instance Show ChessGameState where
    show ChessGameState{board,turn} | turn == White = boardToString board ++ "--+-----------------------\n  | A  B  C  D  E  F  G  H\n" ++ show turn ++ " turn" 
                                    | otherwise = tmp ++ "--+-----------------------\n  | A  B  C  D  E  F  G  H\n" ++ show turn ++ " turn" 
                                        where tmp = unlines (reverse (lines (boardToString board)))


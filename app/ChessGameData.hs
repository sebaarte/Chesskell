{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}



module ChessGameData (ChessGameConfig(..),ChessGameState(..),initialBoard,Command) where
import Board
import Move
import Case
import Player
import Board
import MoveHistory

type Command = String

data ChessGameConfig = ChessGameConfig {pvp::Bool, playWhite::Bool,fileName::FilePath}
data ChessGameState = ChessGameState{board::Board,turn::Player,moveHistory::MoveHistory}

initialBoard = Board ([playerRow White,pawnRow White] ++ replicate 4 emptyRow ++ [pawnRow Black,playerRow Black])



strRow x = unwords (map show x)

instance Show Board where
        show (Board x) = unlines (zipWith (++) leftMargin board)
            where (leftMargin,board) = (map ((++ " |") . show) (reverse [1 .. 8]) , map strRow (reverse x))

instance Show ChessGameState where
    show ChessGameState{board,turn} = show board ++ "--+-----------------------\n  | A  B  C  D  E  F  G  H\nPlayer " ++ show turn ++ " turn" 



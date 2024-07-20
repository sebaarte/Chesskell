{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}


module ChessGameData (ChessGameConfig(..),ChessGameState(..),initialBoard) where
import Board
import Move
import Case
import Player
import Board

data ChessGameConfig = ChessGameConfig {pvp::Bool, playWhite::Bool}
data ChessGameState = ChessGameState{board::Board,turn::Player,lastMove::Move}

initialBoard = Board ([playerRow One,pawnRow One] ++ replicate 4 emptyRow ++ [pawnRow Two,playerRow Two])
emptyState = ChessGameState initialBoard One


strRow x = unwords (map show x)

instance Show Board where
        show (Board x) = unlines (zipWith (++) leftMargin board)
            where (leftMargin,board) = (map ((++ " |") . show) (reverse [1 .. 8]) , map strRow (reverse x))

instance Show ChessGameState where
    show ChessGameState{board,turn} = show board ++ "--+-----------------------\n  | A  B  C  D  E  F  G  H\nPlayer " ++ show turn ++ " turn" 



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

initialBoard = [playerRow White,pawnRow White] ++ replicate 4 emptyRow ++ [pawnRow Black,playerRow Black]
defaultInitialState = (ChessGameState initialBoard White (MoveHistory [] []))






        

instance Show ChessGameState where
    show ChessGameState{board,turn} | turn == White = boardToString board ++ "--+-----------------------\n  | A  B  C  D  E  F  G  H\n" ++ show turn ++ " turn" 
                                    | otherwise = tmp ++ "--+-----------------------\n  | A  B  C  D  E  F  G  H\n" ++ show turn ++ " turn" 
                                        where tmp = unlines (reverse (lines (boardToString board)))


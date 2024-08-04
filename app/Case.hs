{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}


module Case (fromChar,potentialPromotion,Piece(..),Case(..)) where
import Player

data Piece = Empty | Pawn | Rook | Knight | Bishop | Queen | King deriving Eq



data Case = Case {player::Player, piece::Piece} deriving Eq

instance Show Case where
    show Case {piece = Empty, ..} = " ·"
    show Case {piece = Pawn,player}
        | player == White = " p"
        | otherwise = " P"
    show Case {piece = Rook,player}
        | player == White = " r"
        | otherwise = " R"
    show Case {piece = Knight,player}
        | player == White = " n"
        | otherwise = " N"
    show Case {piece = Bishop,player}
        | player == White = " b"
        | otherwise = " B"
    show Case {piece = Queen,player}
        | player == White = " q"
        | otherwise = " Q"
    show Case {piece = King,player}
        | player == White = " k"
        | otherwise = " K"

fromChar:: Char -> Case
fromChar '_' = Case White Empty

-- checks for promotion eligibility and promotes if relevant
potentialPromotion:: Int -> Case -> Case
potentialPromotion row c@(Case player piece) = if (row == 7 && player == White && piece == Pawn) || (row == 0 && player == Black && piece == Pawn) then (Case player Queen) else c
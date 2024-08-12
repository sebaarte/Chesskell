{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}


module Case (potentialPromotion,Piece(..),Case(..),arePiecesDifferent,fromString) where
import Player
import Token

import Data.Char (toLower)

data Piece = Empty | Pawn | Rook | Knight | Bishop | Queen | King deriving (Eq,Show)


-- data type representing a case on the board that is either a piece and a color or empty with no color
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


-- checks for promotion eligibility and promotes if relevant
potentialPromotion:: Int -> Case -> Case
potentialPromotion row c@(Case player piece) = if (row == 7 && player == White && piece == Pawn) || (row == 0 && player == Black && piece == Pawn) then (Case player Queen) else c


arePiecesDifferent:: Case -> Case -> Bool
arePiecesDifferent (Case _ piece1) (Case _ piece2) = piece1 /= piece2

instance Token Piece where
    fromString s = case map (toLower) s of
                        "rook" -> Rook
                        "pawn" -> Pawn
                        "king" -> King
                        "queen" -> Queen
                        "knight" -> Knight
                        "bishop" -> Bishop
                        _ -> Empty
    toString piece | piece ==  Empty = ""
                   | otherwise = show piece
module Player (passTurn,Player(..)) where

data Player = One | Two | None deriving (Eq,Show)
passTurn :: Player -> Player
passTurn player
    | player == One = Two
    | player == Two = One
    | otherwise = None


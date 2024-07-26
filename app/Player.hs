module Player (nextPlayer,Player(..)) where

data Player = One | Two | None deriving (Eq,Show)
nextPlayer :: Player -> Player
nextPlayer player
    | player == One = Two
    | player == Two = One
    | otherwise = None


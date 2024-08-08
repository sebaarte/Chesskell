module Player (otherPlayer,Player(..)) where

data Player = White | Black | None deriving (Eq,Show)
otherPlayer :: Player -> Player
otherPlayer player
    | player == White = Black
    | player == Black = White
    | otherwise = None


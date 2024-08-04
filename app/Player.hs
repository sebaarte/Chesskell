module Player (nextPlayer,Player(..)) where

data Player = White | Black | None deriving (Eq,Show)
nextPlayer :: Player -> Player
nextPlayer player
    | player == White = Black
    | player == Black = White
    | otherwise = None


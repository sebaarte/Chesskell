module Player (otherPlayer,Player(..)) where

-- data type represneting a color / player
data Player = White | Black | None deriving (Eq,Show)

-- passes player turn/ returns other player
otherPlayer :: Player -> Player
otherPlayer player
    | player == White = Black
    | player == Black = White
    | otherwise = None


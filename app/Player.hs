module Player (otherPlayer,Player(..),fromString) where
import Token

-- data type represneting a color / player
data Player = White | Black | None deriving (Eq,Show)

-- passes player turn/ returns other player
otherPlayer :: Player -> Player
otherPlayer player
    | player == White = Black
    | player == Black = White
    | otherwise = None

instance Token Player where
    fromString s = case s of
                "W" -> White
                "w" -> White
                "B" -> Black
                "b" -> Black
                _ -> error "unable to parse Player"
    toString player | player == White = "W"
                    | player == Black = "B"
                    | otherwise = "_"
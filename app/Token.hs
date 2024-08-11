module Token(Token(..)) where


-- a type derived from this class is a parsable token in the chess grammar (i.e. Pos, Move, Player)
class Token a where
    fromString:: String -> a
    toString:: a -> String
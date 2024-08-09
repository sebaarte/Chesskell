module Pos (modifyPos,Pos(..),charToCol,colToChar,substract,(<?),fromStr,isPosBlack) where
import Data.Maybe(fromJust,isJust)


-- Represents a position on the chessboard
data Pos = Pos Int Int deriving (Eq,Show)

-- a generic function to check for limits of a variable x
(<?) :: Ord a => a -> (a,a) -> Bool
(<?) x (min, max) = x >= min && x <= max

-- adds a pair to a position and checks the validity of the result then returns it
modifyPos::Pos -> (Int,Int) -> Maybe Pos
modifyPos (Pos col row) (x,y) = if (x+col) <? (0,7) && (y+row) <? (0,7) then Just (Pos (x+col) (y+row)) else Nothing

-- substract two positions and returns a pair
substract::Pos -> Pos -> (Int,Int)
substract (Pos fromCol fromRow) (Pos toCol toRow) = (toCol - fromCol,toRow - fromRow)

-- maps a char to a column value in the chess board or nothing if the char is invalid
charToCol:: Char -> Maybe Int
charToCol c = case c of
    'a' -> Just 0
    'b' -> Just 1
    'c' -> Just 2
    'd' -> Just 3
    'e' -> Just 4
    'f' -> Just 5
    'g' -> Just 6
    'h' -> Just 7
    _ -> Nothing

-- maps column to char representation
colToChar:: Int -> Maybe Char
colToChar n = case n of
    0 -> Just 'a'
    1 -> Just 'b'
    2 -> Just 'c'
    3 -> Just 'd'
    4 -> Just 'e'
    5 -> Just 'f'
    6 -> Just 'g'
    7 -> Just 'h'
    _ -> Nothing

-- build a pos from string if possible
fromStr:: String -> Maybe Pos
fromStr s@(a:b:_) = if isJust col && length s == 2 then Just (Pos (fromJust col) (read [b]))
                    else Nothing
                    where col = charToCol a

-- is a case located at a position black (not the piece on the case, the case itself)
isPosBlack:: Pos -> Bool
isPosBlack (Pos col row) = even (col+row)


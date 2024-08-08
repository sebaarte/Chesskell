module Pos (modifyPos,Pos(..),charToCol,colToChar,substract,(<?),fromStr,isPosBlack) where
import Data.Maybe(fromJust,isJust)


-- Represents a position on the chessboard
data Pos = Pos Int Int deriving (Eq,Show)

-- a generic function to check for limits of a variable x
(<?) :: Ord a => a -> (a,a) -> Bool
(<?) x (min, max) = x >= min && x <= max

modifyPos::Pos -> (Int,Int) -> Maybe Pos
modifyPos (Pos col row) (x,y) = if (x+col) <? (0,7) && (y+row) <? (0,7) then Just (Pos (x+col) (y+row)) else Nothing

substract::Pos -> Pos -> (Int,Int)
substract (Pos fromCol fromRow) (Pos toCol toRow) = (toCol - fromCol,toRow - fromRow)

-- This could be done in a prettier manner (with a key value mapping) but it's easier to read with a case
-- Binding char representation to column in the chess board
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

-- Binding column to char representation
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

fromStr:: String -> Maybe Pos
fromStr s@(a:b:_) = if isJust col && length s == 2 then Just (Pos (fromJust col) (read [b]))
                    else Nothing
                    where col = charToCol a

isPosBlack:: Pos -> Bool
isPosBlack (Pos col row) = even (col+row)


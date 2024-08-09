module Move (fromString,Move(..),toString) where
import Pos
import Player
import Data.Char (digitToInt)
import Data.Maybe(fromJust,isJust)
import Debug.Trace(trace)

-- data type that encodes a move from a postion to another position
data Move = Move{from::Pos, to::Pos} deriving (Eq,Show)







type ErrorMsg = String

-- converts a string to a move or returns an error msg
fromString:: String -> Either ErrorMsg Move
fromString cmd@(a:b:c:d:_) = if isJust aa && isJust cc
    then 
      Right (Move (Pos (fromJust aa) ((digitToInt b)-1)) (Pos (fromJust cc) ((digitToInt d)-1)))
    else
      (Left "Unable to parse command into valid move")
      where aa = charToCol a
            cc = charToCol c
fromString _ = Left "Unknown data formatting"

-- converts a move to a string or if the move is not convertible, returns nothing
toString:: Move -> Maybe String
toString (Move (Pos fromCol fromRow) (Pos toCol toRow)) = if isJust aa && isJust cc
                                                          then
                                                            Just ([(fromJust aa)] ++ (show (fromRow+1)) ++ [(fromJust cc)] ++ (show (toRow+1)))
                                                          else
                                                            Nothing
                                                          where aa = (colToChar fromCol)
                                                                cc = (colToChar toCol)
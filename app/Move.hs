module Move (fromString,Move(..),toString) where
import Pos
import Player
import Data.Char (digitToInt)
import Data.Maybe(fromJust,isJust)
import Debug.Trace(trace)

data Move = Move{from::Pos, to::Pos} deriving (Eq,Show)







type ErrorMsg = String
fromString:: String -> Either ErrorMsg Move
fromString cmd@(a:b:c:d:_) = if isJust aa && isJust cc
    then 
      Right (Move (Pos (fromJust aa) ((digitToInt b)-1)) (Pos (fromJust cc) ((digitToInt d)-1)))
    else
      trace (show a ++ " " ++ show c) (Left "Unable to parse command into valid move")
      where aa = charToCol a
            cc = charToCol c
fromString _ = Left "Unknown data formatting"

toString:: Move -> Maybe String
toString (Move (Pos fromCol fromRow) (Pos toCol toRow)) = if isJust aa && isJust cc
                                                          then
                                                            Just ([(fromJust aa)] ++ (show (fromRow+1)) ++ [(fromJust cc)] ++ (show (toRow+1)))
                                                          else
                                                            Nothing
                                                          where aa = (colToChar fromCol)
                                                                cc = (colToChar toCol)
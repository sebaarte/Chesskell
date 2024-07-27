module Move (fromString,Move(..),toString) where
import Pos
import Data.Char (digitToInt)
import Data.Maybe(fromJust,isJust)

data Move = Move{from::Pos, to::Pos} deriving (Eq,Show)




type ErrorMsg = String
fromString:: String -> Either ErrorMsg Move
fromString cmd@(a:b:c:d:_) = if length cmd == 4 && isJust aa && isJust cc
    then 
      Right (Move (Pos (fromJust aa) ((digitToInt b)-1)) (Pos (fromJust cc) ((digitToInt d)-1)))
    else
      Left "Unable to parse command into valid move"
      where aa = charToCol a
            cc = charToCol c
fromString _ = Left "Unknown data formatting"

toString:: Move -> Maybe String
toString (Move (Pos fromCol fromRow) (Pos toCol toRow)) = if isJust aa && isJust bb
                                                          then
                                                            Just (show ([(fromJust aa)] ++ (show (fromRow+1)) ++ [(fromJust bb)] ++ (show (toRow+1))))
                                                          else
                                                            Nothing
                                                          where aa = (colToChar fromCol)
                                                                bb = (colToChar toCol)
module SaveFile(saveFile) where
import Data.List
import Debug.Trace

import ChessGameData
import System.IO
import Token
import Player
import MoveHistory
import Board
import Case


playerString:: Player -> String
playerString p = "player(" ++ toString p ++ ")\n"

moveHistoryString:: MoveHistory -> String
moveHistoryString mh@(EntryHistory []) = "history()\n"
moveHistoryString mh@(EntryHistory entries) = "history(" ++  intercalate "," (map toString entries) ++ ")\n"

piecesString:: Board -> String
piecesString board =    let 
                            piecesPos = (allPieces board)
                            cases = map (at board) piecesPos
                        in
                        concat (zipWith (\pos (Case player piece) -> "piece(" ++ toString piece ++ "," ++ toString player ++ "," ++ toString pos ++ ")\n") piecesPos cases)



saveFile:: ChessGameState -> FilePath -> IO ()
saveFile st@ChessGameState{board,turn,moveHistory} f = writeFile f (playerString turn ++ moveHistoryString moveHistory ++ piecesString board)
{-# LANGUAGE PatternGuards #-}


module GrammarReader (parseChessFile) where
import System.IO
import Text.Regex
import Data.Maybe(fromJust,isJust)
import MoveHistory
import Data.List.Split
import Debug.Trace

import Token
import Pos
import Board
import Player
import MovementHandling
import ChessGameData
import Case





-- regexes to parse the grammar
playerRegex:: Regex
playerRegex = mkRegexWithOpts "player\\s*\\((W|B)\\)" False True

historyRegex:: Regex
historyRegex = mkRegexWithOpts "history\\s*\\(\\s*((((rook)|(pawn)|(king)|(queen)|(knight)|(bishop))[a-h][1-8][a-h][1-8]((rook)|(pawn)|(king)|(queen)|(knight)|(bishop))?)(\\s*,\\s*((rook)|(pawn)|(king)|(queen)|(knight)|(bishop))\\s*[a-h][1-8][a-h][1-8]\\s*((rook)|(pawn)|(king)|(queen)|(knight)|(bishop))?\\s*)*\\s*)\\)" False True

moveRegex:: Regex
moveRegex = mkRegexWithOpts "((rook)|(pawn)|(king)|(queen)|(knight)|(bishop))()[a-h][1-8]((rook)|(pawn)|(king)|(queen)|(knight)|(bishop))?" False True

piecesRegex:: Regex
piecesRegex = mkRegexWithOpts "(piece\\s*\\((\\s*([a-z]+)\\s*,\\s*(W|B)\\s*,\\s*([a-h][1-8]\\s*))\\))+" False True


type ErrorMsg = String

parseChessFile:: FilePath -> IO ( Either ErrorMsg ChessGameState )
parseChessFile f = do
                    content <- readFile f
                    let fileLines = lines (content)
                    let playerMatch = matchRegex playerRegex (fileLines !! 0)
                    let historyMatch = matchRegex historyRegex (fileLines !! 1)
                    let piecesMatches = map (matchRegex piecesRegex) (tail (tail fileLines))
                    let board = (fillBoard (map (\x -> (fromJust x) !! 1) piecesMatches))
                    if isJust playerMatch && isJust historyMatch && all (isJust) piecesMatches 
                    then  return (Right (ChessGameState board (playerColor (fromJust (playerMatch) !! 0)) (MoveHistory [] [] ))) 
                    else return (Left "Unable to parse File")

playerColor:: String -> Player
playerColor s | s == "W" || s == "w" = White
              | s == "B" || s == "b" = Black


parsePieceToken:: String -> (Pos,Case)
parsePieceToken s = let str = splitOn "," s
                        player = (fromString (str !! 1))::Player
                        pos = (fromString (str !! 2))::Pos
                        piece = (fromString (str !! 0))::Piece
                    in
                    trace ((toString pos) ++ " " ++ (toString player) ++ " " ++ (toString piece)) (pos,(Case player piece))




fillBoard:: [String] -> Board
fillBoard [] = emptyBoard
fillBoard ss = trace (concat ss) multipleReplace emptyBoard (map parsePieceToken ss)
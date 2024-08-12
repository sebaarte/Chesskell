{-# LANGUAGE PatternGuards #-}


module GrammarReader (parseChessFile) where
import System.IO
import Text.Regex
import Data.Maybe(fromJust,isJust)
import Data.List.Split

import Debug.Trace
import Data.Either.Utils (fromRight)

import Token
import Pos
import Board
import Player
import MovementHandling
import ChessGameData
import Case
import MoveHistory
import Move




-- regexes to parse the grammar
playerRegex:: Regex
playerRegex = mkRegexWithOpts "player\\((W|B)\\)" False False

-- we extended the grammar to include EP and Castle tokens (i.e. en passant and castle moves)
historyRegex:: Regex
historyRegex = mkRegexWithOpts "history\\(((((rook)|(pawn)|(king)|(queen)|(knight)|(bishop))[a-h][1-8][a-h][1-8]((rook)|(pawn)|(king)|(queen)|(knight)|(bishop)|(EP)|(Castle))?)(,((rook)|(pawn)|(king)|(queen)|(knight)|(bishop))[a-h][1-8][a-h][1-8]((rook)|(pawn)|(king)|(queen)|(knight)|(bishop))?)*)\\)" False False


piecesRegex:: Regex
piecesRegex = mkRegexWithOpts "(piece\\((([a-z]+),(W|B),([a-h][1-8]))\\))+" False False


type ErrorMsg = String

parseChessFile:: FilePath -> IO ( Either ErrorMsg ChessGameState )
parseChessFile f = do
                    content <- readFile f
                    let fileLines = lines ( filter (/= ' ') content)
                    let playerMatch = matchRegex playerRegex (fileLines !! 0)
                    let historyMatch = matchRegex historyRegex (fileLines !! 1)
                    let piecesMatches = map (matchRegex piecesRegex) (tail (tail fileLines))
                    let board = (fillBoard (map (\x -> (fromJust x) !! 1) piecesMatches))
                    if isJust playerMatch && isJust historyMatch && all (isJust) piecesMatches 
                    then  return (Right (ChessGameState board (playerColor (fromJust (playerMatch) !! 0)) (parseMoveHistory ((fromJust historyMatch) !! 0)))) 
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
                    (pos,(Case player piece))




fillBoard:: [String] -> Board
fillBoard [] = emptyBoard
fillBoard ss = multipleReplace emptyBoard (map parsePieceToken ss)





-- un-Haskell way to parse tokens but way easier that breaking down the function into smaller functions
parseMoveToken:: String -> MoveHistoryEntry
parseMoveToken s = fromString s
                    
                    

parseMoveHistory:: String -> MoveHistory
parseMoveHistory s = let tokens = splitOn "," s in EntryHistory (map (parseMoveToken) tokens)





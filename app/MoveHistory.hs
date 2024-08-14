
module MoveHistory (isEmpty,MoveHistory(..),appendMove,lastMove,hasKingMoved,hasKingSideRookMoved,hasQueenSideRookMoved,MoveHistoryEntry(..),MoveType(..),determineSpecialMove,canUndo) where
import Data.Char (toLower)
import Text.Regex
import Data.Maybe(fromJust,isJust)
import Data.Either.Utils (fromRight)
import Data.List
import Debug.Trace


import Move
import Case
import Board
import Player
import Pos
import Token

-- data type to represent a move history in a chess game (useful for castling and en passant moves)
-- the first data constructor is not used anymore but is left there
data MoveHistory = MoveHistory [Move] [(Case,Case)] | EntryHistory [MoveHistoryEntry] 


data MoveHistoryEntry = Entry Move (Piece,Piece) MoveType deriving (Eq)

data MoveType = EnPassant | Castle | Normal deriving Eq

determineSpecialMove:: Board -> Move -> MoveType
determineSpecialMove board move@(Move from to) = let diff@(colDiff,rowDiff) = (substract from to)
                                            in
                                            case (pieceAt board from) of
                                            King -> if (abs colDiff) == 2 then Castle else Normal
                                            Pawn -> if (abs colDiff) == (abs rowDiff) && (pieceAt board to) == Empty then EnPassant else Normal
                                            _ -> Normal
                                            

-- adds a move and the relevant information to the move history
appendMove:: MoveHistory -> Board -> Move -> MoveHistory
appendMove (MoveHistory moves cases) board move@(Move from to) =  MoveHistory (moves ++ [move]) (cases ++ [((at board from),(at board to))])
appendMove (EntryHistory entries) board move@(Move from to) = EntryHistory (entries ++ [Entry move ((pieceAt board from),(pieceAt board to)) (determineSpecialMove board move)]) 

-- get the last move in the move history
lastMove:: MoveHistory -> Move
lastMove (MoveHistory moves cases) = last moves
lastMove (EntryHistory entries) = let (Entry move _ _) = last entries in move



-- checks move history to see if a king already moved for castling purposes
hasKingMoved:: MoveHistory -> Board -> Player -> Bool
hasKingMoved (MoveHistory moves cases) board player = (elem (Case player King) fromMoves) where fromMoves = map fst cases
hasKingMoved (EntryHistory entries) board player =  all (==False) (map (\x -> isAKingMove x board player) entries)

isAKingMove:: MoveHistoryEntry -> Board -> Player -> Bool
isAKingMove (Entry move@(Move from to) (piece1,piece2) moveType) board player = piece1 == King && (playerAt board from) == player

-- checks move history to see if a king side rook already moved for castling purposes
hasKingSideRookMoved:: MoveHistory -> Board -> Player -> Bool
hasKingSideRookMoved (MoveHistory moves cases) board White = (elem (Pos 7 0) fromMoves) || (at board (Pos 7 0)) /= (Case White Rook) where fromMoves = map from moves
hasKingSideRookMoved (MoveHistory moves cases) board Black = (elem (Pos 7 7) fromMoves) || (at board (Pos 7 7)) /= (Case Black Rook) where fromMoves = map from moves
hasKingSideRookMoved (EntryHistory entries) board White = any (==True) (map (\x -> isAKingSideRookMove x board White) entries) || (at board (Pos 7 0)) /= (Case White Rook)
hasKingSideRookMoved (EntryHistory entries) board White = any (==True) (map (\x -> isAKingSideRookMove x board Black) entries ) || (at board (Pos 7 7)) /= (Case Black Rook)

isAKingSideRookMove:: MoveHistoryEntry -> Board -> Player -> Bool
isAKingSideRookMove (Entry move@(Move from to) (piece1,piece2) moveType) board White = from /= (Pos 7 0)
isAKingSideRookMove (Entry move@(Move from to) (piece1,piece2) moveType) board Black = from /= (Pos 7 7)

-- checks move history to see if a queen side rook already moved for castling purposes
hasQueenSideRookMoved:: MoveHistory -> Board -> Player -> Bool
hasQueenSideRookMoved (MoveHistory moves cases) board White = (elem (Pos 0 0) fromMoves) || (at board (Pos 0 0)) /= (Case White Rook) where fromMoves = map from moves
hasQueenSideRookMoved (MoveHistory moves cases) board Black = (elem (Pos 0 7) fromMoves) || (at board (Pos 0 7)) /= (Case Black Rook) where fromMoves = map from moves
hasQueenSideRookMoved (EntryHistory entries) board White = (any (==True) (map (\x -> isAQueenSideRookMove x board White) entries)) || (at board (Pos 0 0)) /= (Case White Rook)
hasQueenSideRookMoved (EntryHistory entries) board White = (any (==True) (map (\x -> isAQueenSideRookMove x board Black) entries)) || (at board (Pos 0 7)) /= (Case Black Rook)


isAQueenSideRookMove:: MoveHistoryEntry -> Board -> Player -> Bool
isAQueenSideRookMove (Entry move@(Move from to) (piece1,piece2) moveType) board White = from /= (Pos 0 0)
isAQueenSideRookMove (Entry move@(Move from to) (piece1,piece2) moveType) board Black = from /= (Pos 0 7)

isEmpty:: MoveHistory -> Bool
isEmpty (MoveHistory [] []) = True
isEmpty (MoveHistory move cases) = False
isEmpty (EntryHistory []) = True
isEmpty (EntryHistory entries) = False

canUndo:: MoveHistory -> Bool
canUndo (EntryHistory entries) = length entries > 1

instance Token MoveType where
    fromString s | map toLower s == "ep" = EnPassant
                 | map toLower s  == "castle" = Castle
                 | otherwise = Normal
    toString sm | sm == Castle = "Castle"
                | sm == EnPassant = "ep"
                | otherwise = ""


-- regex to match a move in a history
moveRegex:: Regex
moveRegex = mkRegexWithOpts "((rook)|(pawn)|(king)|(queen)|(knight)|(bishop))([a-h][1-8][a-h][1-8])((rook)|(pawn)|(king)|(queen)|(knight)|(bishop)|(EP)|(Castle))?" False False

instance Token MoveHistoryEntry where
    fromString s = do
                    let ptokens = matchRegex moveRegex s
                    let tokens = (filter (/= "")(fromJust ptokens))
                    case length (tokens) of
                        3 -> (Entry (fromRight (fromStr (tokens !! 2))) ((fromString (tokens !! 0)),Empty) Normal)
                        5 -> (Entry (fromRight (fromStr (tokens !! 2))) ((fromString (tokens !! 0)), fromString (tokens !! 3)) (fromString (tokens !! 3)))
                        _ -> (error "unable to parse Move object")
    toString (Entry move (piece1,piece2) moveType) = (toString piece1) ++ (fromJust (toStr move)) ++ if moveType == Normal then toString piece2 else toString moveType
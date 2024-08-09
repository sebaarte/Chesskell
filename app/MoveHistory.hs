
module MoveHistory (MoveHistory(..),appendMove,lastMove,hasKingMoved,hasKingSideRookMoved,hasQueenSideRookMoved) where
import Move
import Case
import Board
import Player
import Pos

-- data type to represent a move history in a chess game (useful for castling and en passant moves)
data MoveHistory = MoveHistory [Move] [(Case,Case)] deriving (Show)

-- not used in this part of the project
data MoveHistoryEntry = Entry Move (Case,Case)

-- adds a move and the relevant information to the move history
appendMove:: MoveHistory -> Board -> Move -> MoveHistory
appendMove (MoveHistory moves cases) board move@(Move from to) =  MoveHistory (moves ++ [move]) (cases ++ [((at board from),(at board to))])

-- get the last move in the move history
lastMove:: MoveHistory -> Move
lastMove (MoveHistory moves cases) = last moves

-- checks move history to see if a king already moved for castling purposes
hasKingMoved:: MoveHistory -> Player -> Bool
hasKingMoved (MoveHistory moves cases) player = (elem (Case player King) fromMoves) where fromMoves = map fst cases

-- checks move history to see if a king side rook already moved for castling purposes
hasKingSideRookMoved:: MoveHistory -> Board -> Player -> Bool
hasKingSideRookMoved (MoveHistory moves cases) board White = (elem (Pos 0 7) fromMoves) || (at board (Pos 0 7)) /= (Case White Rook) where fromMoves = map from moves
hasKingSideRookMoved (MoveHistory moves cases) board Black = (elem (Pos 7 0) fromMoves) || (at board (Pos 7 0)) /= (Case Black Rook) where fromMoves = map from moves

-- checks move history to see if a queen side rook already moved for castling purposes
hasQueenSideRookMoved:: MoveHistory -> Board -> Player -> Bool
hasQueenSideRookMoved (MoveHistory moves cases) board White = (elem (Pos 0 0) fromMoves) || (at board (Pos 0 0)) /= (Case White Rook) where fromMoves = map from moves
hasQueenSideRookMoved (MoveHistory moves cases) board Black = (elem (Pos 7 7) fromMoves) || (at board (Pos 7 7)) /= (Case Black Rook) where fromMoves = map from moves


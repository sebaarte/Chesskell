
module MoveHistory (MoveHistory(..),appendMove,lastMove,hasKingMoved,hasKingSideRookMoved,hasQueenSideRookMoved) where
import Move
import Case
import Board
import Player
import Pos


data MoveHistory = MoveHistory [Move] [(Case,Case)]

appendMove:: MoveHistory -> Board -> Move -> MoveHistory
appendMove (MoveHistory moves cases) board move@(Move from to) =  MoveHistory (moves ++ [move]) (cases ++ [((at board from),(at board to))])

lastMove:: MoveHistory -> Move
lastMove (MoveHistory moves cases) = last moves

hasKingMoved:: MoveHistory -> Player -> Bool
hasKingMoved (MoveHistory moves cases) player = not (elem (Case player King) fromMoves) where fromMoves = map fst cases


hasKingSideRookMoved:: MoveHistory -> Board -> Player -> Bool
hasKingSideRookMoved (MoveHistory moves cases) board One = not (elem (Pos 0 7) fromMoves) && (at board (Pos 0 7)) /= (Case One Rook) where fromMoves = map from moves
hasKingSideRookMoved (MoveHistory moves cases) board Two = not (elem (Pos 7 0) fromMoves) && (at board (Pos 7 0)) /= (Case Two Rook) where fromMoves = map from moves


hasQueenSideRookMoved:: MoveHistory -> Board -> Player -> Bool
hasQueenSideRookMoved (MoveHistory moves cases) board One = not (elem (Pos 0 0) fromMoves) && (at board (Pos 0 0)) /= (Case One Rook) where fromMoves = map from moves
hasQueenSideRookMoved (MoveHistory moves cases) board Two = not (elem (Pos 7 7) fromMoves) && (at board (Pos 7 7)) /= (Case Two Rook) where fromMoves = map from moves
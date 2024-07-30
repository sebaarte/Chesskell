
module MoveHistory (MoveHistory(..),appendMove,lastMove) where
import Move
import Case
import Board


data MoveHistory = MoveHistory [Move] [(Case,Case)]

appendMove:: MoveHistory -> Board -> Move -> MoveHistory
appendMove (MoveHistory moves cases) board move@(Move from to) =  MoveHistory (moves ++ [move]) (cases ++ [((at board from),(at board to))])

lastMove:: MoveHistory -> Move
lastMove (MoveHistory moves cases) = last moves
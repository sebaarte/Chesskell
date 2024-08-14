{-# LANGUAGE ViewPatterns #-} -- we use this extension to be able to pattern match on lists

module Undo (undoLastMove) where
import Board
import ChessGameData
import MoveHistory
import MovementHandling
import Player
import Pos
import Case

undoLastMove:: ChessGameState -> ChessGameState
undoLastMove st@ChessGameState{board,turn,moveHistory} =    let (EntryHistory (reverse -> (lMove:stlMove:prev))) = moveHistory
                                                                tempBoard = unApplyMove board lMove (otherPlayer turn)
                                                            in
                                                            (ChessGameState (unApplyMove tempBoard stlMove turn) turn (EntryHistory (reverse prev)))


unApplyMove:: Board -> MoveHistoryEntry -> Player -> Board
unApplyMove board (Entry move@(Move from to) (taking,taken) Normal) player =  let playerReplace = if taken == Empty then None else (otherPlayer player)
                                                                              in
                                                                              multipleReplace board  [(from,(at board to)),(to,(Case playerReplace taken))]

unApplyMove board (Entry move@(Move from@(Pos fromCol fromRow) to@(Pos toCol toRow)) (taking,taken) Castle) player = case substract from to of
                                                                                                                        (2,0) -> multipleReplace board  [(from,(at board to)),(to,(Case None Empty)),((Pos 5 fromRow),(Case None Empty)),((Pos 7 fromRow),(Case player Rook))]
                                                                                                                        (-2,0) -> multipleReplace board  [(from,(at board to)),(to,(Case None Empty)),((Pos 3 fromRow),(Case None Empty)),((Pos 0 fromRow),(Case player Rook))]
                                                                                                                        _ -> error "invalid castle move"
unApplyMove board (Entry move@(Move from@(Pos fromCol fromRow) to@(Pos toCol toRow)) (taking,taken) EnPassant) player =  case player of
                                                                                                                            White -> multipleReplace board [(from,(at board to)),(to,(Case None Empty)),((Pos toCol (toRow-1)),(Case Black Pawn))]
                                                                                                                            Black -> multipleReplace board [(from,(at board to)),(to,(Case None Empty)),((Pos toCol (toRow-1)),(Case White Pawn))]
                                                                                                                            _ -> error "invalid en passant move"
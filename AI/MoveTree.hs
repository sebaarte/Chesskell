module MoveTree() where
import Board
import MovementHandling
import ChessGameData
import Player


data MoveTree = Root ChessGameState | Leaf Board | Level Board [MoveTree]


fromState:: ChessGameState -> MoveTree
fromState st@ChessGameState{board,turn} =  Level board (map Leaf (map (applyMove board) (possibleMoves st)))

fromBoardPlayer::Player -> Board -> MoveTree
fromBoardPlayer turn board =  Level board (map Leaf (map (applyMove board) (possibleMoves ChessGameState{board,turn})))

fromBoard:: Board -> MoveTree
fromBoard board = fromBoardPlayer Two board


constructTree:: MoveTree -> Int -> MoveTree
constructTree st@ChessGameState{board,turn} 0 = 

constructTree (Level (Board board) possibleStates) depth = 

constructTree (Root st@ChessGameState{board,turn}) depth = 


module MoveTree(MoveTree(..),constructTree,getLastMove) where
import Board
import MovementHandling
import ChessGameData
import Player
import MoveHistory

import Control.Parallel.Strategies
import Debug.Trace


data MoveTree = Root ChessGameState | Leaf ChessGameState | Level ChessGameState [MoveTree]

---------------------------------------------------

leafFromMove:: ChessGameState -> Move -> MoveTree
leafFromMove st@ChessGameState{board,turn,moveHistory} move = Leaf (ChessGameState (applyMove board move) (nextPlayer turn) (appendMove moveHistory board move))


--levelFromState:: ChessGameState -> MoveTree
levelFromState st@(ChessGameState board turn moveHistory) =  Level st ((map (leafFromMove st) (possibleMoves st)) )

leavesFrom:: MoveTree -> [MoveTree]
leavesFrom (Root st@ChessGameState{board,turn,moveHistory}) = let (Level _ leaves) = levelFromState st
                                                                    in leaves

leavesFrom (Leaf st@ChessGameState{board,turn,moveHistory}) = let (Level _ leaves) = levelFromState st
                                                                    in leaves



constructTree:: MoveTree -> Int -> MoveTree
constructTree leaf@(Leaf st@ChessGameState{board,turn,moveHistory}) 0 = levelFromState st

constructTree leaf@(Leaf st@ChessGameState{board,turn,moveHistory}) depth = let nextStates =  (leavesFrom leaf)
                                                                            in 
                                                                            --Level st ( parMap rpar (\x -> constructTree x (depth-1)) nextStates)
                                                                            Level st ( map (\x -> constructTree x (depth-1)) nextStates)


constructTree root@(Root st@ChessGameState{board,turn,moveHistory}) depth = let nextStates =  (leavesFrom root)
                                                                            in 
                                                                            --Level st ( parMap rpar (\x -> constructTree x (depth-1)) nextStates)
                                                                            trace ( show (length nextStates)) Level st (( map (\x -> constructTree x (depth-1)) nextStates) `using` parListChunk 100 rpar)


getLastMove:: MoveTree -> Move
getLastMove (Root st@ChessGameState{board,turn,moveHistory}) = lastMove moveHistory
getLastMove (Leaf st@ChessGameState{board,turn,moveHistory}) = lastMove moveHistory
getLastMove (Level st@ChessGameState{board,turn,moveHistory} _) = lastMove moveHistory

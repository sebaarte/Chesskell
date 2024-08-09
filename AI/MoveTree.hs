module MoveTree(MoveTree(..),constructTree,getLastMove) where
import Board
import MovementHandling
import ChessGameData
import Player
import MoveHistory

import Control.Parallel.Strategies
import Debug.Trace

-- type used to represent a tree of possible moves
-- Root is the tree root i.e. the beginning of the tree without successors
-- A leaf is a move that is located at the depth of the algorithm (i.e. the end of the tree)
-- a level is simply a state and its successors
data MoveTree = Root ChessGameState | Leaf ChessGameState | Level ChessGameState [MoveTree]

---------------------------------------------------

-- build a move tree leaf based on a chess game state -> used to build leaves of a move tree
leafFromMove:: ChessGameState -> Move -> MoveTree
leafFromMove st@ChessGameState{board,turn,moveHistory} move = Leaf (ChessGameState (applyMove board move) (otherPlayer turn) (appendMove moveHistory board move))

-- build a move tree level from a state. A move tree level
levelFromState:: ChessGameState -> MoveTree
levelFromState st@(ChessGameState board turn moveHistory) =  Level st ((map (leafFromMove st) (possibleMoves st)) )

-- given a root or a leaf, build the succesors
leavesFrom:: MoveTree -> [MoveTree]
leavesFrom (Root st@ChessGameState{board,turn,moveHistory}) = let (Level _ leaves) = levelFromState st
                                                                    in leaves

leavesFrom (Leaf st@ChessGameState{board,turn,moveHistory}) = let (Level _ leaves) = levelFromState st
                                                                    in leaves


-- build a tree recursively starting from a root
constructTree:: MoveTree -> Int -> MoveTree
constructTree leaf@(Leaf st@ChessGameState{board,turn,moveHistory}) 0 = levelFromState st

constructTree leaf@(Leaf st@ChessGameState{board,turn,moveHistory}) depth = let nextStates =  (leavesFrom leaf)
                                                                            in 
                                                                            --Level st ( parMap rpar (\x -> constructTree x (depth-1)) nextStates)
                                                                            Level st ( map (\x -> constructTree x (depth-1)) nextStates)


constructTree root@(Root st@ChessGameState{board,turn,moveHistory}) depth = let nextStates =  (leavesFrom root)
                                                                            in 
                                                                            --Level st ( parMap rpar (\x -> constructTree x (depth-1)) nextStates)
                                                                            Level st (( map (\x -> constructTree x (depth-1)) nextStates) `using` parListChunk 100 rpar)

-- return the last move of a node in the tree (used to extract the move from the tree and play it)
getLastMove:: MoveTree -> Move
getLastMove (Root st@ChessGameState{board,turn,moveHistory}) = lastMove moveHistory
getLastMove (Leaf st@ChessGameState{board,turn,moveHistory}) = lastMove moveHistory
getLastMove (Level st@ChessGameState{board,turn,moveHistory} _) = lastMove moveHistory

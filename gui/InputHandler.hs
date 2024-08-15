module InputHandler(inputHandler) where
import Graphics.Gloss
import Debug.Trace
import Graphics.Gloss.Interface.Pure.Game

import Pos
import GUIdata
import ChessGameData
import Board
import Move
import Game
import Undo


inputHandler:: Event -> GUIstate -> GUIstate
inputHandler  (EventKey (MouseButton LeftButton) Down _ (x, y)) (GUIstate st Nothing) = let pos = (Pos (absToRel x) (absToRel y))
                                                                                        in
                                                                                        trace ("x= " ++ (show x) ++ " y= " ++ (show y))
                                                                                        (if isInBounds pos then
                                                                                        (GUIstate st (Just (pos,(at (board st) pos))))
                                                                                        else 
                                                                                        if x <? (220,300) && y <? (0,50) then (GUIstate (undoLastMove st) Nothing)
                                                                                                    else (GUIstate st Nothing))

inputHandler  (EventKey (MouseButton LeftButton) Down _ (x, y)) (GUIstate st (Just (from,c))) = let to = (Pos (absToRel x) (absToRel y))
                                                                                                    move = (Move from to)
                                                                                                in
                                                                                                trace ("x= " ++ (show x) ++ " y= " ++ (show y))
                                                                                                (if isInBounds to
                                                                                                then
                                                                                                (GUIstate (nextState st move) Nothing)
                                                                                                else
                                                                                                    if x <? (180,260) && y <? (-25,25) then (GUIstate (undoLastMove st) Nothing)
                                                                                                    else (GUIstate st Nothing))
inputHandler _ st = st
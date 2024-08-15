module GUIdata(GUIstate(..),squareSize,offset,absToRel,relToAbs) where
import ChessGameData
import Pos
import Case

squareSize = 50::Float
offset = 50::Float

-- converts a relative column or row into absolute one
relToAbs:: Int -> Float
relToAbs x = (-200)+(a*offset) where a = fromIntegral x


absToRel:: Float -> Int
absToRel x = round ((x+200)/offset)



data GUIstate = GUIstate ChessGameState (Maybe (Pos,Case))
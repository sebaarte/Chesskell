{-# LANGUAGE RecordWildCards #-}


module Render (render,caseToFile) where
import Data.Char (toLower)
import Graphics.Gloss
import Pos
import GUIdata
import Board
import Player
import Token
import Case
import ChessGameData
import Graphics.Gloss.Juicy
import System.IO.Unsafe (unsafePerformIO)
import Game
import MovementHandling





caseGrey:: Color
caseGrey = makeColorI 192 192 192 255

brown:: Color
brown = makeColorI 205 127 50 255

translucidBlack:: Color
translucidBlack = makeColorI 0 0 0 122

whiteCase:: Picture
whiteCase = color caseGrey $ rectangleSolid 50 50

blackCase:: Picture
blackCase = color brown $ rectangleSolid squareSize squareSize



oddRow:: [Picture]
oddRow = concat (replicate 4 [blackCase,whiteCase])

evenRow:: [Picture]
evenRow = concat (replicate 4 [whiteCase,blackCase])

relativePositions:: [(Int,Int)]
relativePositions = [(x,y) | x <- [0..7], y <- [0..7]]

absolutePositions:: [(Float,Float)]
absolutePositions = map (\(x,y) -> (relToAbs x,relToAbs y)) relativePositions

chessBoard:: [Picture]
chessBoard = zipWith (\(x,y) cases -> translate x y $ cases) absolutePositions (concat (replicate 4 (oddRow ++ evenRow)))

caseToFile:: Case -> FilePath
caseToFile (Case player piece) = "imgs/" ++ map toLower (show piece) ++ "-" ++ toString player ++ ".png" 


zipperCaseAndPictures:: (Pos,Case) -> Picture
zipperCaseAndPictures ((Pos col row),c@(Case player piece)) =  translate (relToAbs col) (relToAbs row) $ scale 0.4 0.4 $ png (caseToFile c)

png :: FilePath -> Picture
png fname = maybe (text "PNG ERROR") id (unsafePerformIO $ loadJuicyPNG fname)

gameText:: ChessGameState -> Picture
gameText st = translate (-300) (200) $ scale 0.5 0.5 $ color red $ (Text "Game ongoing")
-- gameText st@ChessGameState{board,turn,moveHistory}  | isWinner turn st = translate (-300) (200) $ scale 0.3 0.3 $ color red $ (Text ("Player " ++ show turn ++ " won the game"))
--                                                     | isWinner (otherPlayer turn) st = translate (-300) (200) $ scale 0.3 0.3 $ color red $ (Text ("Player " ++ show (otherPlayer turn) ++ " won the game"))
--                                                     | isDraw st = translate (-300) (200) $ scale 0.5 0.5 $ color red $ (Text "Game drawn")
--                                                     | otherwise =  translate (-300) (200) $ scale 0.5 0.5 $ color red $ (Text "Game ongoing")

undoButton:: [Picture]
undoButton = [translate (220) (0) $ color green $ rectangleSolid 80 50] ++ [translate (220) (0) $ scale 0.2 0.2 $ png "imgs/undo_button.png"]

highlight:: Pos -> Picture
highlight (Pos col row) = translate (relToAbs col) (relToAbs row) $ color translucidBlack $ circleSolid 12

render:: GUIstate -> Picture
render (GUIstate st@(ChessGameState board turn moveHistory) Nothing) = let  pieces = allPiecesInGame board
                                                                            piecesImages = map zipperCaseAndPictures pieces
                                                                    in
                                                                    pictures (((chessBoard) ++ piecesImages) ++ [gameText st] ++ undoButton)

render (GUIstate st@(ChessGameState board turn moveHistory) (Just (pos,c))) = let   pieces = allPiecesInGame board
                                                                                    piecesImages = map zipperCaseAndPictures pieces
                                                                                    highlighted = map highlight (possibleDestinationsFrom st pos c)
                                                                    in
                                                                    pictures (((chessBoard) ++ piecesImages) ++ [gameText st] ++ highlighted ++ undoButton)

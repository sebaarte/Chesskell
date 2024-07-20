module Board (charToCol,colToChar,playerAt,Board(..),pawnRow,emptyRow,playerRow,at,locateKing,allPositions,locateAllPlayerPieces,piecesInGame) where

import Case
import Player
import Pos

import Data.Either (isLeft,isRight)
import Data.Maybe(fromJust,isJust)
import Data.List (elemIndex,findIndex,find)




initialRow = [Rook,Knight,Bishop,Queen,King,Bishop,Knight,Rook]

allPositions = [ (Pos x y) | x <- [0..7], y <- [0..7]]
    

emptyRow = replicate 8 (Case None Empty)

pawnRow player =  replicate 8 (Case player Pawn)
playerRow player =  (map (Case player) initialRow)

newtype Board = Board [[Case]]

locateKingRow::  Player ->[Case] -> Int
locateKingRow player row = let res = elemIndex (Case player King) row
                            in if isJust res then fromJust res else 10
                           

locateKing:: Board -> Player -> Pos
locateKing (Board board) player = let res = map (locateKingRow player) board
                                     in Pos (fromJust(find (/=10) res)) (fromJust(findIndex (/=10) res))


locateAllPlayerPieces:: Board -> Player -> [Pos]
locateAllPlayerPieces board player = filter (\x -> (playerAt board x) == player) allPositions

allPieces::Board -> [Pos]
allPieces board = locateAllPlayerPieces board One ++ locateAllPlayerPieces board Two

piecesInGame:: Board -> [Case]
piecesInGame board = map (at board) (allPieces board)

rowAt::Board -> Pos -> [Case]
rowAt (Board board) (Pos _ row) = board !! row

at::Board -> Pos -> Case
at b@(Board board) m@(Pos col row) = let cases = rowAt b m in cases !! col

playerAt:: Board -> Pos -> Player
playerAt board pos = player where (Case player _ ) = at board pos


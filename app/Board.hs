module Board (charToCol,colToChar,playerAt,Board(..),pawnRow,emptyRow,playerRow,at,locateKing,allPositions,locateAllPlayerPieces,piecesInGame,boardToString,allPieces,emptyBoard,pieceAt) where

import Case
import Player
import Pos

import Data.Either (isLeft,isRight)
import Data.Maybe(fromJust,isJust)
import Data.List (elemIndex,findIndex,find)


-- the board type is simply a matrix of Cases
type Board = [[Case]]


-- a row of pieces
initialRow:: [Piece]
initialRow = [Rook,Knight,Bishop,Queen,King,Bishop,Knight,Rook]

-- the list of all positions in the board
allPositions = [ (Pos x y) | x <- [0..7], y <- [0..7]]
    
-- empty row used to build the initial board
emptyRow = replicate 8 (Case None Empty)

-- an empty board for testing and debugging purposes
emptyBoard = replicate 8 emptyRow

-- a row made of pawns used to build the initial board
pawnRow player =  replicate 8 (Case player Pawn)

-- returns a starting row for a player
playerRow:: Player -> [Case]
playerRow player =  (map (Case player) initialRow)



-- helper function for locateKing function
locateKingRow::  Player ->[Case] -> Int
locateKingRow player row = let res = elemIndex (Case player King) row
                            in if isJust res then fromJust res else 10
                           
-- locate a player's king
locateKing:: Board -> Player -> Pos
locateKing board player = let res = map (locateKingRow player) board
                                     in Pos (fromJust(find (/=10) res)) (fromJust(findIndex (/=10) res))

-- returns the positions of all pieces of a player
locateAllPlayerPieces:: Board -> Player -> [Pos]
locateAllPlayerPieces board player = filter (\x -> (playerAt board x) == player) allPositions

-- returns the positions of all pieces in the board
allPieces::Board -> [Pos]
allPieces board = locateAllPlayerPieces board White ++ locateAllPlayerPieces board Black

-- returns remaining pieces in the board without positions
piecesInGame:: Board -> [Case]
piecesInGame board = map (at board) (allPieces board)

-- helper function for the "at" function
rowAt::Board -> Pos -> [Case]
rowAt board (Pos _ row) = board !! row

-- returns the Case at the given position
at::Board -> Pos -> Case
at board m@(Pos col row) = let cases = rowAt board m in cases !! col

-- returns the color of the piece at a given position (or None if the case is empty)
playerAt:: Board -> Pos -> Player
playerAt board pos = player where (Case player _ ) = at board pos

pieceAt:: Board -> Pos -> Piece
pieceAt board pos = piece where (Case _ piece) = at board pos

strRow x = unwords (map show x)
-- converts a board to a string. Since board is not a data, we can't use show
-- we do not instantiate board as a token since we do not want it to have a fromString representation
boardToString:: Board -> String
boardToString b = unlines (zipWith (++) leftMargin board)
                        where (leftMargin,board) = (map ((++ " |") . show) (reverse [1 .. 8]) , map strRow (reverse b))
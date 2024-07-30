{-# LANGUAGE RecordWildCards #-}


module MovementHandling (isPossibleDestination,isValidMove,validCases,applyMove,Move(..),fromString,isCheckmate,isDraw,possibleMoves,isWinner) where

import Case
import Pos
import Board
import Player
import ChessGameData
import Move
import MoveHistory

import Debug.Trace (trace)
import Data.Maybe(fromJust,isJust)
import Data.List

-- generic function to replace put an element val in a list l at index n
replace::[a] -> Int -> a -> [a]
replace l n val = take n l ++ val : drop (n+1) l


replaceInBoard:: Board -> Pos -> Case -> Board
replaceInBoard b@(Board board) pos@(Pos col row) c@(Case player piece) = Board (replace board row (replace (board !! row) col c))

multipleReplace:: Board -> [(Pos,Case)] -> Board
multipleReplace b@(Board board) [] = b
multipleReplace b@(Board board) (x:xs) = multipleReplace (replaceInBoard b (fst x) (snd x)) xs

-- move a piece from position "from" to position "to" leaving an empty case in the "from" position
applyMove:: Board -> Move -> Board
applyMove b@(Board board) (Move from@(Pos 4 fromRow) to@(Pos toCol toRow)) =  case (toCol,(at b from)) of
                                                                                (6,(Case One King)) -> multipleReplace b [(from,(Case None Empty)),(to,(Case One King)),((Pos 5 0),(Case One Rook)),((Pos 7 0),(Case None Empty) )]
                                                                                (2,(Case One King)) -> multipleReplace b [(from,(Case None Empty)),(to,(Case One King) ),((Pos 3 0),(Case One Rook) ),((Pos 0 0),(Case None Empty) )]
                                                                                (6,(Case Two King)) -> multipleReplace b [(from,(Case None Empty)),(to,(Case Two King) ),((Pos 5 7),(Case Two Rook) ),((Pos 7 7),(Case None Empty) )]
                                                                                (2,(Case Two King)) -> multipleReplace b [(from,(Case None Empty)),(to,(Case Two King) ),((Pos 3 7),(Case Two Rook) ),((Pos 0 7),(Case None Empty) )]
                                                                                _ -> replaceInBoard tempBoard from (Case None Empty) 
                                                                                        where tempBoard = replaceInBoard b to (at b from)

applyMove b@(Board board) (Move from@(Pos fromCol fromRow) to@(Pos toCol toRow)) = replaceInBoard tempBoard from (Case None Empty) 
    where tempBoard = replaceInBoard b to (at b from)

-- returns False if the piece to be moved is not belonging to the current player or if the destination already contains another piece of the current player
validCases:: ChessGameState -> Move -> Bool
validCases state@ChessGameState{board,turn} Move{ from, to} = (playerAt board from) == turn && (playerAt board to) /= turn

-- checks wether a move is in the chessboard bounds
moveInBounds::Move -> Bool
moveInBounds move@(Move (Pos fromCol fromRow) (Pos toCol toRow)) = all (==True) (map (\x -> x <? (0,7)) [fromCol,fromRow,toCol,toRow])

isChecked:: ChessGameState -> Bool
isChecked state@ChessGameState{board,turn,..} = (movesFrom state (locateKing board turn) turn) /= []



-- checks for validity of a move in all possible cases
isValidMove:: ChessGameState -> Move -> Bool
isValidMove state@ChessGameState{board,turn,moveHistory} move@(Move from to) = do   let nextBoard = applyMove board move
                                                                                        res = moveInBounds move
                                                                                            && (validCases state move)
                                                                                            && (isPossibleDestination state move (at board from))
                                                                                            && not (isChecked (ChessGameState nextBoard turn (appendMove moveHistory board move))) 
                                                                                    res
                                                                    


-- possibles directions for rook, bishops and queens
diagonals = [(1,1),(-1,1),(1,-1),(-1,-1)]
cardinals = [(1,0),(0,1),(-1,0),(0,-1)]

linesFrom:: Board -> Pos -> Player -> [(Int,Int)]-> Int ->[Pos]
linesFrom b@(Board board) p@(Pos col row) player directions 4 = concat (map (linesFrom b p player directions) [0..3])
linesFrom b@(Board board) p@(Pos col row) player directions n = do
                                                         let newPos = modifyPos p (directions !! n)
                                                         if (playerAt b p) == nextPlayer player
                                                         then 
                                                            []
                                                         else 
                                                            if isJust newPos && playerAt b (fromJust newPos) /= player
                                                                then (fromJust newPos) : linesFrom b (fromJust newPos) player directions n 
                                                            else []


movesFrom:: ChessGameState -> Pos -> Player -> [Pos]
movesFrom state@ChessGameState{board,turn,..} p@(Pos col row) player = filter (\x -> isPossibleDestination state (Move x p) (at board x)) allPositions

possibleMoves::ChessGameState -> [Move]
possibleMoves state@ChessGameState{board,turn,..} = let pieces = locateAllPlayerPieces board turn
                                                        evaluatedDestinations =  allPositions \\ pieces
                                                        res = filter (isValidMove state) [(Move from to) | from <- pieces,to <- evaluatedDestinations ]  in
                                                            res

isCheckmate:: ChessGameState -> Bool
isCheckmate state@ChessGameState{board,turn,..}= isChecked state && (possibleMoves state) == []



isWinner:: Player -> ChessGameState -> Bool
isWinner player (ChessGameState board _ moveHistory) = isCheckmate ChessGameState{board,turn,moveHistory}
                        where turn = nextPlayer player

isDraw:: ChessGameState -> Bool
isDraw state = (possibleMoves state) == []

-- given a move and a case, assess the validity of the move based on movement possibilities and special rules (blocked by other pieces,en passant, castling etc)
isPossibleDestination:: ChessGameState -> Move -> Case -> Bool
isPossibleDestination _ _ (Case None Empty) = False
isPossibleDestination state@ChessGameState{board,turn,moveHistory} move@(Move from@(Pos _ fromRow) to) (Case One Pawn) = case substract from to of
                                                                                (0,1) -> at board to == (Case None Empty)
                                                                                (1,1) -> playerAt board to == Two
                                                                                (-1,1) -> playerAt board to == Two
                                                                                (0,2) -> playerAt board to == None && playerAt board (fromJust (modifyPos from (0,1))) == None && fromRow == 1
                                                                                _ -> False

isPossibleDestination state@ChessGameState{board,turn,moveHistory} move@(Move from@(Pos _ fromRow) to) (Case Two Pawn) = case substract from to of
                                                                                (0,-1) -> at board to == (Case None Empty)
                                                                                (-1,-1) -> playerAt board to == One
                                                                                (1,-1) -> playerAt board to == One
                                                                                (0,-2) -> playerAt board to == None && playerAt board (fromJust (modifyPos from (0,-1))) == None && fromRow == 6
                                                                                _ -> False

isPossibleDestination state@ChessGameState{board,..} move@(Move from to) (Case player Knight) = case substract from to of 
                                                                                                    (1,2)-> playerAt board to /= player
                                                                                                    (-1,2)-> playerAt board to /= player
                                                                                                    (1,-2)-> playerAt board to /= player
                                                                                                    (-1,-2)-> playerAt board to /= player
                                                                                                    (2,1) -> playerAt board to /= player
                                                                                                    (-2,1)-> playerAt board to /= player
                                                                                                    (2,-1)-> playerAt board to /= player
                                                                                                    (-2,-1)-> playerAt board to /= player
                                                                                                    _ -> False
isPossibleDestination state@ChessGameState{board,..} move@(Move from to) (Case player Bishop) = elem to (linesFrom board from player diagonals 4)
isPossibleDestination state@ChessGameState{board,..} move@(Move from to) (Case player Rook) = elem to (linesFrom board from player cardinals 4)
isPossibleDestination state@ChessGameState{board,..} move@(Move from to) (Case player Queen) = elem to ((linesFrom board from player cardinals 4) ++ (linesFrom board from player diagonals 4))
isPossibleDestination state@ChessGameState{board,..} move@(Move from to) (Case player King) = elem (Just to) (map (modifyPos from) (cardinals ++ diagonals)) || isCastleMovePossible state move



isCastleMovePossible:: ChessGameState -> Move -> Bool
isCastleMovePossible st@ChessGameState{board,..} move@(Move from to@(Pos 6 toRow)) = (at board (Pos 5 toRow)) == (Case None Empty)  
                                                                                && (at board (Pos 6 toRow)) == (Case None Empty) 
                                                                                && not (hasKingMoved moveHistory turn) && not (hasKingSideRookMoved moveHistory board turn)
                                                                                && not (isChecked st)

isCastleMovePossible st@ChessGameState{board,..} move@(Move from to@(Pos 2 toRow)) = (at board (Pos 3 toRow)) == (Case None Empty) 
                                                                                && (at board (Pos 2 toRow)) == (Case None Empty) 
                                                                                && not (hasKingMoved moveHistory turn) && not (hasQueenSideRookMoved moveHistory board turn)
                                                                                && not (isChecked st)
isCastleMovePossible _ _ = False



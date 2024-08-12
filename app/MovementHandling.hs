{-# LANGUAGE RecordWildCards #-}


module MovementHandling (isPossibleDestination,isValidMove,validCases,applyMove,Move(..),fromString,isCheckmate,isDraw,possibleMoves,isWinner,multipleReplace,replaceInBoard) where

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

-- generic function to put an element val in a list l at index n replacing the element at index n
replace::[a] -> Int -> a -> [a]
replace l n val = take n l ++ val : drop (n+1) l

-- replaces a the case at a certain position in the board with another case
replaceInBoard:: Board -> Pos -> Case -> Board
replaceInBoard board pos@(Pos col row) c@(Case player piece) = replace board row (replace (board !! row) col c)

-- sequentially replace cases in the board with the provided position and case tuples
multipleReplace:: Board -> [(Pos,Case)] -> Board
multipleReplace b [] = b
multipleReplace b (x:xs) = multipleReplace (replaceInBoard b (fst x) (snd x)) xs

-- move a piece from position "from" to position "to" or apply a special move (castle, en passant)
applyMove:: Board -> Move -> Board
-- castling move
applyMove b (Move from@(Pos 4 fromRow) to@(Pos toCol toRow)) =  case (toCol,(at b from)) of
                                                                                (6,(Case White King)) -> multipleReplace b [(from,(Case None Empty)),(to,(Case White King)),((Pos 5 0),(Case White Rook)),((Pos 7 0),(Case None Empty) )]
                                                                                (2,(Case White King)) -> multipleReplace b [(from,(Case None Empty)),(to,(Case White King) ),((Pos 3 0),(Case White Rook) ),((Pos 0 0),(Case None Empty) )]
                                                                                (6,(Case Black King)) -> multipleReplace b [(from,(Case None Empty)),(to,(Case Black King) ),((Pos 5 7),(Case Black Rook) ),((Pos 7 7),(Case None Empty) )]
                                                                                (2,(Case Black King)) -> multipleReplace b [(from,(Case None Empty)),(to,(Case Black King) ),((Pos 3 7),(Case Black Rook) ),((Pos 0 7),(Case None Empty) )]
                                                                                _ -> replaceInBoard tempBoard from (Case None Empty) 
                                                                                        where tempBoard = replaceInBoard b to (at b from)
-- en passant
applyMove b (Move from@(Pos fromCol 3) to@(Pos toCol 2)) =  if (at b from) == (Case Black Pawn) && (at b to) == (Case None Empty) && (fromCol - toCol) /= 0
                                                            then multipleReplace b [(from,(Case None Empty)),(to,(Case Black Pawn)),((Pos toCol 3),(Case None Empty))]
                                                            else replaceInBoard tempBoard from (Case None Empty) 
                                                                where tempBoard = replaceInBoard b to (at b from)
applyMove b (Move from@(Pos fromCol 4) to@(Pos toCol 5)) =  if (at b from) == (Case White Pawn) && (at b to) == (Case None Empty) && (fromCol - toCol) /= 0 
                                                            then multipleReplace b [(from,(Case None Empty)),(to,(Case White Pawn)),((Pos toCol 4),(Case None Empty))]
                                                            else replaceInBoard tempBoard from (Case None Empty) 
                                                                where tempBoard = replaceInBoard b to (at b from)

-- normal move
applyMove b (Move from@(Pos fromCol fromRow) to@(Pos toCol toRow)) = replaceInBoard tempBoard from (Case None Empty) 
    where tempBoard = replaceInBoard b to (potentialPromotion toRow (at b from))



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
isValidMove state@ChessGameState{board,turn,moveHistory} move@(Move from to) = let nextBoard = applyMove board move
                                                                                   res = moveInBounds move
                                                                                        && (validCases state move)
                                                                                        && (isPossibleDestination state move (at board from))
                                                                                        && not (isChecked (ChessGameState nextBoard turn (appendMove moveHistory board move))) 
                                                                                in res
                                                                    


-- possibles directions for rook, bishops and queens
diagonals = [(1,1),(-1,1),(1,-1),(-1,-1)]
cardinals = [(1,0),(0,1),(-1,0),(0,-1)]

-- given a position, directions and a player, gives the possible lines from that position according to the given directions.
-- a bit complicated but easy to use for different directions and merging of different directions
linesFrom:: Board -> Pos -> Player -> [(Int,Int)]-> Int ->[Pos]
linesFrom b p@(Pos col row) player directions 4 = concat (map (linesFrom b p player directions) [0..3])
linesFrom b p@(Pos col row) player directions idx = do
                                                         let newPos = modifyPos p (directions !! idx)
                                                         if (playerAt b p) == otherPlayer player
                                                         then 
                                                            []
                                                         else 
                                                            if isJust newPos && playerAt b (fromJust newPos) /= player
                                                                then (fromJust newPos) : linesFrom b (fromJust newPos) player directions idx 
                                                            else []

-- returns the possible positions from which a move is possible to the given position -> used to assess a check position
movesFrom:: ChessGameState -> Pos -> Player -> [Pos]
movesFrom state@ChessGameState{board,turn,..} p@(Pos col row) player = filter (\x -> isPossibleDestination state (Move x p) (at board x)) allPositions

-- return all possible moves for a player in a game state
possibleMoves::ChessGameState -> [Move]
possibleMoves state@ChessGameState{board,turn,..} = let pieces = locateAllPlayerPieces board turn
                                                        evaluatedDestinations =  allPositions \\ pieces
                                                        res = filter (isValidMove state) [(Move from to) | from <- pieces,to <- evaluatedDestinations ]  in
                                                            res

-- checks wether or not the current player is in a checkmate position
isCheckmate:: ChessGameState -> Bool
isCheckmate state@ChessGameState{board,turn,..}= isChecked state && (possibleMoves state) == []


-- checks wether a player is a winner
isWinner:: Player -> ChessGameState -> Bool
isWinner player (ChessGameState board _ moveHistory) = isCheckmate ChessGameState{board,turn,moveHistory}
                        where turn = otherPlayer player


isDraw:: ChessGameState -> Bool
isDraw state = isStalemate state || isMaterialInsufficient state


isStalemate:: ChessGameState -> Bool
isStalemate state = (possibleMoves state) == [] 

-- checks all the possible cases for a insufficient material ending
isMaterialInsufficient:: ChessGameState -> Bool
isMaterialInsufficient state@ChessGameState{board,turn,moveHistory} =   let positions = (allPieces board)
                                                                            noKings = filter (\x -> arePiecesDifferent (Case None King) (at board x)) positions
                                                                            cases = map (at board) noKings
                                                                        in
                                                                        case length noKings of
                                                                        0 -> True
                                                                        1 -> elem (Case White Bishop) cases || elem (Case Black Bishop) cases || elem (Case White Knight) cases || elem (Case Black Knight) cases
                                                                        2 -> elem (Case White Bishop) cases && elem (Case Black Bishop) cases && (isPosBlack (noKings !! 0) == isPosBlack (noKings !! 1))
                                                                        _ -> False

-- given a move and a case, assess the validity of the move based on movement possibilities and special rules (blocked by other pieces,en passant, castling etc)
-- very ugly function but works
isPossibleDestination:: ChessGameState -> Move -> Case -> Bool
isPossibleDestination _ _ (Case None Empty) = False
isPossibleDestination state@ChessGameState{board,turn,moveHistory} move@(Move from@(Pos _ fromRow) to) (Case White Pawn) = case substract from to of
                                                                                (0,1) -> at board to == (Case None Empty)
                                                                                (1,1) -> playerAt board to == Black || isEnPassant state move
                                                                                (-1,1) -> playerAt board to == Black || isEnPassant state move
                                                                                (0,2) -> playerAt board to == None && playerAt board (fromJust (modifyPos from (0,1))) == None && fromRow == 1
                                                                                _ -> False

isPossibleDestination state@ChessGameState{board,turn,moveHistory} move@(Move from@(Pos _ fromRow) to) (Case Black Pawn) = case substract from to of
                                                                                (0,-1) -> at board to == (Case None Empty)
                                                                                (-1,-1) -> playerAt board to == White || isEnPassant state move
                                                                                (1,-1) -> playerAt board to == White || isEnPassant state move
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
isPossibleDestination state@ChessGameState{board,..} move@(Move from to) (Case player King) = elem (Just to) (map (modifyPos from) (cardinals ++ diagonals)) || isCastleMove state move


-- checks if a move is a castle move and its validity
isCastleMove:: ChessGameState -> Move -> Bool
isCastleMove st@ChessGameState{board,..} move@(Move from to@(Pos 6 toRow)) =(caseOneEmpty && caseTwoEmpty &&  kingMove && rookMove && checked && toValid)
                                                                                    where   caseOneEmpty = (at board (Pos 5 toRow)) == (Case None Empty)
                                                                                            caseTwoEmpty = (at board (Pos 6 toRow)) == (Case None Empty)
                                                                                            kingMove = not (hasKingMoved moveHistory turn)
                                                                                            rookMove = not (hasKingSideRookMoved moveHistory board turn)
                                                                                            checked = not (isChecked st)
                                                                                            toValid = toRow == 0 || toRow == 7 

isCastleMove st@ChessGameState{board,..} move@(Move from to@(Pos 2 toRow)) = (caseOneEmpty && caseTwoEmpty &&  kingMove && rookMove && checked && toValid)
                                                                                    where   caseOneEmpty = (at board (Pos 3 toRow)) == (Case None Empty)
                                                                                            caseTwoEmpty = (at board (Pos 2 toRow)) == (Case None Empty)
                                                                                            kingMove = not (hasKingMoved moveHistory turn)
                                                                                            rookMove = not (hasQueenSideRookMoved moveHistory board turn)
                                                                                            checked = not (isChecked st)
                                                                                            toValid = toRow == 0 || toRow == 7 
isCastleMove _ _ = False

-- checks if a move is a en passant move and its validity
isEnPassant:: ChessGameState -> Move -> Bool
isEnPassant st@ChessGameState{board,turn,moveHistory} move@(Move from to@(Pos _ toRow)) = let   (Move prevFrom prevTo) = lastMove moveHistory
                                                                                                prevDiff = substract prevFrom prevTo
                                                                                                pawnsDiff = substract prevTo  from
                                                                                                diagonalDiff = if turn == Black then (0,-1) else (0,1)
                                                                                    in
                                                                                    (at board prevTo) == (Case (otherPlayer turn) Pawn) && (prevDiff == (0,2) || prevDiff == (0,-2)) -- check if pawn moved 2 cases
                                                                                    && (pawnsDiff == (1,0) || pawnsDiff == (-1,0)) -- taking pawn is located next to taken pawn
                                                                                    && substract prevTo to == diagonalDiff -- the taking pawn is moving to the correct case

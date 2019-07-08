module Lib where

import Data.Maybe (listToMaybe)

type Square = (Int, Int)

data PieceType = Pawn | Rook | Knight | Bishop | Queen | King
                 deriving (Eq, Show)

data PieceColor = White | Black
                  deriving (Eq, Show)

data Piece = Piece { piecetype :: PieceType
                   , piececolor :: PieceColor
                   , square :: Square
                   }
                   deriving (Eq, Show)

type Board = [Piece]

validSquare :: Square -> Bool
validSquare (x, y) = x < 8 && x >= 0 && y < 8 && y >= 0

validMove :: Board -> Piece -> Square -> Bool
validMove b p sq@(x', y') = 
    let (x, y) = square p in
    validSquare sq &&
    case piecetype p of
        Pawn -> let dir = if piececolor p == White then 1 else -1 in
                if pieceAt b sq == Nothing
                    then  if x == dir || x == 8 + dir
                          then x == x' && y + 2 * dir == y' 
                          else x == x' && y + dir == y'
                    else abs (x - x') == 1 && y + dir == y'
        
        Rook -> (x == x') /= (y == y') -- TODO implement colision checking
        
        Knight -> (abs (x - x') == 2 && abs (y - y') == 1) ||
                  (abs (x - x') == 1 && abs (y - y') == 2)
        
        Bishop -> abs (x - x') == abs (y - y') -- TODO implement collision checking
        
        Queen -> validMove b (Piece Rook (piececolor p) (square p)) sq ||
          
                 validMove b (Piece Bishop (piececolor p) (square p)) sq

        King -> abs (x - x') < 2 && abs (y - y') < 2 && square p /= sq

pieceAt :: Board -> Square -> Maybe Piece
pieceAt b sq = listToMaybe $ filter ((==sq) . square) b

standardBoard :: Board
standardBoard = error "TODO implement"

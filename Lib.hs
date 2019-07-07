module Lib where

data PieceType = Pawn | Rook | Knight | Bishop | Queen | King
data PieceColor = White | Black

data Piece = Piece { piecetype :: PieceType
                   , piececolor :: PieceColor
                   , x :: Int
                   , y :: Int 
                   }

type Board = [Piece]

standardBoard :: Board
standardBoard = error "TODO implement"

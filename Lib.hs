module Lib where

import Data.Maybe (listToMaybe)
import Control.Monad.State
import Data.List (and)

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
type Move = (Piece, Square)
type GameState = State (Board, PieceColor)

validSquare :: Square -> Bool
validSquare (x, y) = x < 8 && x >= 0 && y < 8 && y >= 0

-- TODO debug and test this, compiler doesn't know
-- the rules of chess
validMove :: Board -> Move -> Bool
validMove b (p, sq@(x', y')) = 
    let (x, y) = square p in
    validSquare sq &&
    case piecetype p of
        Pawn -> let dir = if piececolor p == White then 1 else -1 in
                if pieceAt b sq == Nothing
                    then x == x' && 
                         if y == dir || y == 8 + dir
                          then y + dir == y' || y + 2 * dir == y' 
                          else y + dir == y'
                    else abs (x - x') == 1 && y + dir == y'
        
        Rook -> (x == x') /= (y == y') &&  -- TODO implement colision checking
                and (map (\k -> pieceAt b k == Nothing || k == (x,y) || k == sq) 
                [(i, j) | i <- intsBetween x x', j <- intsBetween y y'])
        
        Knight -> (abs (x - x') == 2 && abs (y - y') == 1) ||
                  (abs (x - x') == 1 && abs (y - y') == 2)
        
        Bishop -> abs (x - x') == abs (y - y') -- TODO implement collision checking
        
        Queen -> validMove b ((Piece Rook (piececolor p) (square p)), sq) ||
          
                 validMove b ((Piece Bishop (piececolor p) (square p)), sq)

        King -> abs (x - x') < 2 && abs (y - y') < 2 && square p /= sq

    where intsBetween m n = if m < n then [m..n] else [n..m]

pieceAt :: Board -> Square -> Maybe Piece
pieceAt b sq = listToMaybe $ filter ((==sq) . square) b

removePiece :: Piece -> Board -> Board
removePiece p = filter (/=p)

addPiece :: Piece -> Board -> Board
addPiece = (:)

movePiece :: Move -> GameState ()
movePiece m@(p@(Piece t c _), sq) = 
    do (b, col) <- get
       let enemy = pieceAt b sq
       let removeEnemy = case enemy of
                         Nothing -> id
                         Just e -> removePiece e
       if validMove b m
       then put (addPiece (Piece t c sq) $ removePiece p $ removeEnemy $ b
                , if col == White then Black else White
             )
       else return ()
     
move :: (Square, Square) -> GameState ()
move (f, t) = 
    do (b, col) <- get
       let p = pieceAt b f
       case p of
           Nothing -> return ()
           Just (p@(Piece _ c _)) -> 
               if c == col
                   then movePiece (p, t)
                   else return ()
                          

standardBoard :: Board
standardBoard = let w = map (\i -> Piece Pawn White (i, 1)) [0..7] ++         
                      [ Piece Rook White (0, 0)
                      , Piece Rook White (7, 0)
                      , Piece Knight White (1, 0)
                      , Piece Knight White (6, 0)
                      , Piece Bishop White (2, 0)
                      , Piece Bishop White (5, 0)
                      , Piece Queen White (3, 0)
                      , Piece King White (4, 0)
                      ] in
                      w ++ map (\(Piece t _ (x, y)) -> Piece t Black (x, 7 - y)) w

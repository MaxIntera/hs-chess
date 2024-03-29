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
type GameState = (Board, PieceColor)
type GameOp = State GameState

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
                         if y == dir || y == 7 + dir
                          then y + dir == y' || y + 2 * dir == y' 
                          else y + dir == y'
                    else abs (x - x') == 1 && y + dir == y'
        
        Rook -> (x == x') /= (y == y') &&
                all (\k -> pieceAt b k == Nothing || k == (x,y) || k == sq) 
                [(i, j) | i <- intsBetween x x', j <- intsBetween y y']
        
        Knight -> (abs (x - x') == 2 && abs (y - y') == 1) ||
                  (abs (x - x') == 1 && abs (y - y') == 2)
        
        Bishop -> abs (x - x') == abs (y - y') &&
                all (\k -> pieceAt b k == Nothing || k == (x,y) || k == sq) 
                (zip (intsBetween x x') (intsBetween y y'))
        
        Queen -> validMove b ((Piece Rook (piececolor p) (square p)), sq) ||
          
                 validMove b ((Piece Bishop (piececolor p) (square p)), sq)

        King -> abs (x - x') < 2 && abs (y - y') < 2 && square p /= sq

    where intsBetween m n = if m < n then [m..n] else [m, m-1..n]

pieceAt :: Board -> Square -> Maybe Piece
pieceAt b sq = listToMaybe $ filter ((==sq) . square) b

removePiece :: Piece -> Board -> Board
removePiece p = filter (/=p)

addPiece :: Piece -> Board -> Board
addPiece = (:)

movePiece :: Move -> GameOp (Bool, String)
movePiece m@(p@(Piece t c _), sq) = 
    do (b, col) <- get
       let enemy = pieceAt b sq
       let removeEnemy = case enemy of
                         Nothing -> id
                         Just e -> removePiece e
       if validMove b m
       then put (addPiece (Piece t c sq) $ removePiece p $ removeEnemy $ b
                , if col == White then Black else White
             ) >> return (True, "Success")
       else return $ (False, "Error: Invalid move \"" ++ show p ++ " to " ++ show sq ++ "\"")

move :: (Square, Square) -> GameOp (Bool, String)
move (f, t) = 
    do (b, col) <- get
       let p = pieceAt b f
       case p of
           Nothing -> return $ (False, "Error: No piece at " ++ show f)
           Just (p@(Piece _ c _)) -> 
               if c == col
                   then movePiece (p, t)
                   else return $ (False, "Error: Invalid piece color") 
                          

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

inCheck :: Board -> Square -> PieceColor -> Bool
inCheck b sq c = 
    any (\p -> 
        fst $ evalState (movePiece (p, sq)) (b, if c == White then Black else White)
        ) (filter (\p -> piececolor p /= c) b)

getKing :: Board -> PieceColor -> Maybe Piece
getKing b c = listToMaybe $ filter (\(Piece t c' _) -> t == King && c' == c) b

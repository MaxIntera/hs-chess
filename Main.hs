import Lib
import Control.Monad.State

main = do
    putStr $ showBoard (standardBoard, White)
    putStr . showBoard $ execState demoGame (standardBoard, White)

demoGame = move ((3, 1), (3, 2))

sqToChar :: Board -> Square -> Char
sqToChar b sq  =  case pieceAt b sq of
                  Nothing -> ' '
                  Just (Piece t _ _) -> head $ show t

showBoard :: (Board, PieceColor) -> String
showBoard (b, c) = "\n____\n" ++ show c ++ "\n" ++
                (concat $ map (\row -> 
                '|' : map (\col -> 
                           sqToChar b (col, row)
                          ) [0..7] ++ "|\n"   
                ) [0..7])

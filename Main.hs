import Lib
import Control.Monad.State
import Data.List (intercalate)

main = do
    putStr $ showBoard (standardBoard, White)
    let (str, s) = runState demoGame (standardBoard, White)
    putStrLn . showBoard $ s
    putStrLn str

demoGame = liftM (intercalate "\n") . mapM move $
              [ ((1,1), (1,2))
              , ((0,6), (0,5))
              , ((2,0), (0,2))
              , ((1,6), (1,4))
              , ((0,2), (3,5))
              , ((1,4), (1,3))
              , ((3,5), (2,6))
              , ((3,7), (2,6))
              ] 

testMove :: Board -> ((Int, Int), (Int, Int)) -> IO ()
testMove b sqs = putStrLn . showBoard $ execState (move sqs) (b, White)

sqToChar :: Board -> Square -> Char
sqToChar b sq  =  case pieceAt b sq of
                  Nothing -> ' '
                  Just (Piece t _ _) -> if t == Knight then 'N' else head $ show t

showBoard :: (Board, PieceColor) -> String
showBoard (b, c) = "\n____\n" ++ show c ++ "\n" ++
                (concat $ map (\row -> 
                show row ++ '|' : map (\col -> 
                           sqToChar b (col, row)
                          ) [0..7] ++ "|\n"   
                ) [7,6..0] ) 
                ++ "  " ++ 
                concat (map show [0..7]) ++ "\n"

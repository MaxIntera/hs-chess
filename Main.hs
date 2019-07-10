import Lib
import Control.Monad.State
import Data.List (intercalate)

main = do
    putStr $ showBoard (standardBoard, White)
    let (str, s) = runState demoGame (standardBoard, White)
    putStrLn . showBoard $ s
    putStrLn str

demoGame = liftM (intercalate "\n") . mapM move $
              [ ((0,1), (0,3))
              , ((0,6), (0,5))
              , ((0,0), (0,2))
              , ((1,6), (1,4))
              , ((0,2), (4,2))
              , ((1,4), (0,3))
              , ((4,2), (4,5))
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
                '|' : map (\col -> 
                           sqToChar b (col, row)
                          ) [0..7] ++ "|\n"   
                ) [0..7])

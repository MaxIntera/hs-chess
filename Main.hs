import Lib
import Control.Monad.State
import Data.List (intercalate)
import Text.ParserCombinators.ReadP
import Data.Char

main =
    return (standardBoard, White) >>=
    iterateM interactGame

    where iterateM m = m >=> iterateM m

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

showBoard :: GameState -> String
showBoard (b, c) = "\n____\n" ++ show c ++ "\n" ++
                (concat $ map (\row -> 
                show (row + 1) ++ '|' : map (\col -> 
                           sqToChar b (col, row)
                          ) [0..7] ++ "|\n"   
                ) [7,6..0] ) 
                ++ "  " ++ ['A'..'H'] ++ "\n"

interactGame :: GameState -> IO GameState
interactGame s =do 
    putStrLn $ showBoard s
    str <- getLine
    let maybemv = parseMaybe parseMove str
    case maybemv of
        Nothing -> return s
        Just mv -> return $ execState (move mv) s

charToNum c = ord c - ord 'A'

parseMove :: ReadP (Square, Square)
parseMove = do
    skipSpaces
    x <- charToNum <$> satisfy (\c -> c >= 'A' && c <= 'H')
    y <- digitToInt <$> satisfy (\c -> c >= '1' && c <= '8')
    string " to "
    x' <- charToNum <$> satisfy (\c -> c >= 'A' && c <= 'H')
    y' <- digitToInt <$> satisfy (\c -> c >= '1' && c <= '8')
    skipSpaces
    return ((x,y - 1),(x',y' - 1))

parseMaybe :: ReadP a -> String -> Maybe a
parseMaybe parser input =
    case readP_to_S parser input of
        [] -> Nothing
        ((result, _):_) -> Just result

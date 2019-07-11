import Lib
import Control.Monad.State
import Data.List (intercalate)
import Text.ParserCombinators.ReadP
import Data.Char
import System.Environment (getArgs)

main = do
    args <- getArgs
    let i = length args > 0 && head args == "-i"

    return (standardBoard, White) >>= iterateM (interactGame i)

    where iterateM m = m >=> iterateM m

demoGame = liftM (intercalate "\n" . map show) . mapM move $
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
testMove b sqs = putStrLn . showBoard False $ execState (move sqs) (b, White)

sqToChar :: Bool -> Board -> Square -> Char
sqToChar i b sq  =  case pieceAt b sq of
                  Nothing -> ' '
                  Just (Piece t c _) -> 
                      if (c == White) /= i
                      then case t of
                          Pawn -> '\9817'
                          Rook -> '\9814'
                          Knight -> '\9816'
                          Bishop -> '\9815'
                          Queen -> '\9813'
                          King -> '\9812'
                      else case t of
                          Pawn -> '\9823'
                          Rook -> '\9820'
                          Knight -> '\9822'
                          Bishop -> '\9821'
                          Queen -> '\9819'
                          King -> '\9818'


showBoard :: Bool -> GameState -> String
showBoard i (b, c) = "\n____\n" ++ show c ++ "\n" ++
                (concat $ map (\row -> 
                show (row + 1) ++ '|' : map (\col -> 
                           sqToChar i b (col, row)
                          ) [0..7] ++ "|\n"   
                ) [7,6..0] ) 
                ++ "  " ++ ['A'..'H'] ++ "\n"
                ++ if check then "Your king is in check!\n" else "" 
    where
        check = case square <$> getKing b c of
            Nothing -> False
            Just sq -> inCheck b sq c

interactGame :: Bool -> GameState -> IO GameState
interactGame i s = do 
    putStrLn $ showBoard i s
    str <- getLine
    let maybemv = parseMaybe parseMove (toLower <$> str)
    case maybemv of
        Nothing -> return s
        Just mv -> return $ execState (move mv) s

charToNum c = ord c - ord 'a'

parseMove :: ReadP (Square, Square)
parseMove = do
    skipSpaces
    x <- charToNum <$> satisfy (\c -> c >= 'a' && c <= 'h')
    y <- digitToInt <$> satisfy (\c -> c >= '1' && c <= '8')
    string " to "
    x' <- charToNum <$> satisfy (\c -> c >= 'a' && c <= 'h')
    y' <- digitToInt <$> satisfy (\c -> c >= '1' && c <= '8')
    skipSpaces
    return ((x,y - 1),(x',y' - 1))

parseMaybe :: ReadP a -> String -> Maybe a
parseMaybe parser input =
    case readP_to_S parser input of
        [] -> Nothing
        ((result, _):_) -> Just result

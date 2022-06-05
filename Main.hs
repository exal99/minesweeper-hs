module Main where

import Data.Char
import Data.List
import Data.Maybe
import System.Random
import Control.Monad

infix 1 ?
infix 2 :?

data Cell = Mine | Empty Int | Flagged Cell | Hidden Cell 

type Pos = (Int, Int)
type Board a = Pos -> a

instance Show Cell where
    show Mine = "*"
    show (Empty i) = if i==0 then "-" else show i
    show (Flagged c) = "^"
    show (Hidden c) = " "


data Action a = (Pos -> Bool) :? (a -> a)

(?) :: Board a -> Action a -> Board a
(b ? (prej :? act)) p = if prej p then act val else val where
    val = b p 

applyRule :: (Cell -> a) -> Cell -> a
applyRule f (Flagged c) = f c
applyRule f (Hidden c) = f c
applyRule f c = f c

isEmpty :: Cell -> Bool
isEmpty (Empty _) = True
isEmpty (Mine) = False
isEmpty c = applyRule isEmpty c

isHidden :: Cell -> Bool
isHidden (Hidden _) = True
isHidden _ = False

isFlagged :: Cell -> Bool
isFlagged (Flagged _) = True
isFlagged _ = False

cellNum :: Cell -> Int
cellNum (Empty n) = n
cellNum Mine = -1
cellNum c = applyRule cellNum c


minefield :: Int -> Int -> [Pos] -> Board Cell
minefield xsize ysize mines = board where
    field' :: Board Cell
    field' (x,y) = if (x >= 0 && y >= 0 && x < xsize && y < ysize) then Hidden (Empty 0) else Empty (-4)
    
    placeMine :: Pos -> Board Cell -> Board Cell
    placeMine pos board = (board ? (==pos) :? const (Hidden Mine)) ? 
                          (\p -> (isEmpty . board $ p) && (isNeighbor pos p)) :? inc
                          where
                            isNeighbor (a,b) (x,y) = d == 1 || d == 2 where
                                d = (a-x)*(a-x) + (b-y)*(b-y)

                            inc (Empty n) = Empty (n+1)
                            inc (Hidden c) = Hidden (inc c)
                            inc c = c
    
    board = foldr placeMine field' mines
    

revealCell :: Cell -> Cell
revealCell (Hidden c) = c
revealCell x = x

reveal :: Pos -> Board Cell -> Board Cell
reveal p b = b ? (==p) :? revealCell

flagCell :: Cell -> Cell
flagCell (Hidden c) = Flagged c
flagCell c = c

unflagCell :: Cell -> Cell
unflagCell (Flagged c) = Hidden c
unflagCell c = c

flag :: Pos -> Board Cell -> Board Cell
flag pos b = b ? (isHidden (b pos) &&) . (==pos)  :? flagCell
    
unflag :: Pos -> Board Cell -> Board Cell
unflag pos b = b ? (isHidden (b pos) &&) . (==pos) :? unflagCell

toggleFlag :: Pos -> Board Cell -> Board Cell
toggleFlag pos b = if isFlagged $ b pos then unflag pos b else flag pos b 


revealRecursive :: Pos -> Board Cell -> Board Cell
revealRecursive pos@(x,y) b
    | isHidden cell && cellNum cell == 0 = foldr revealRecursive (reveal pos b) neighbors
    | isHidden cell && cellNum cell /= 0 = revealed
    | otherwise = b where
        revealed = reveal pos b
        cell = b pos
        neighbors = [(x + dx,y + dy) | dx <- [-1..1], dy <- [-1..1], (dx, dy) /= (0,0)]

revealAll :: Board Cell -> Board Cell
revealAll b = revealCell . b

showBoard :: Show a => Int -> Int -> Board a -> String
showBoard xsize ysize b = header ++ (foldl (\rest (ch, line) -> rest ++ (ch : " |" ++ line ++ "|\n")) "" $ zip (enumFrom 'A') rows) where
                              rows = [intercalate "|" [show . b $ (x,y) | x <- [0..xsize - 1]]| y <- [0..ysize - 1]]
                              header = "   " ++ (intersperse ' ' . take xsize $ enumFrom '0') ++ "\n"

type Parser a = [String] -> Maybe (a, [String])

parseWord :: Parser String
parseWord [] = Nothing
parseWord (x:xs) = Just (x, xs)


parsePos :: Parser Pos
parsePos input = do
    (ypos, rest) <- parseWord input
    (xpos, rest') <- parseWord rest
    return (((ord . head) xpos - (ord '0'), (ord . head) ypos - (ord 'A')), rest')

parseAction :: Parser (Pos -> Board Cell -> Board Cell)
parseAction input = do
    (action, rest) <- parseWord input
    case action of
        "R"  -> Just (revealRecursive, rest)
        "F" -> Just (toggleFlag, rest)
        _ -> Nothing


executeLine :: [String] -> Board Cell -> Board Cell
executeLine input state = fromMaybe state executedAction where
    executedAction = do
        (action, r) <- parseAction input  
        (pos, r') <- parsePos r
        if r' /= [] then
            Nothing
        else
            return $ action pos state

main :: IO ()
main = do
    minePos <- genMines [] mines
    loop $ minefield xsize ysize minePos
    where
        xsize = 10
        ysize = 10
        mines = 10
        xrand = getStdRandom $ randomR (0,xsize - 1)
        yrand = getStdRandom $ randomR (0,ysize - 1)

        loop field = do
            putStrLn $ showBoard xsize ysize field
            putStr "Action >>> "
            inp <- getLine
            loop $ executeLine (words . map toUpper $ inp) field

        genMines :: [Pos] -> Int -> IO [Pos]
        genMines m 0 = return m
        genMines m n = do
            xpos <- (xrand :: IO Int)
            ypos <- (yrand :: IO Int)
            if not $ (xpos, ypos) `elem` m then
                genMines ((xpos,ypos):m) (n-1)
            else
                genMines m n

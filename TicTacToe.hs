module TicTacToe where

import Data.Char
import Data.Maybe
import Data.List
import Text.Read

-------------------------------------------------------------------
data Player = O | X
            deriving (Eq, Show)

data Cell = Empty | Taken Player
          deriving (Eq, Show)

type Board = ([Cell], Int)

type Position = (Int, Int)

-------------------------------------------------------------------

--
-- Some useful functions from, or based on, the unassessed problem sheets...
--

-- Preserves Just x iff x satisfies the given predicate. In all other cases
-- (including Nothing) it returns Nothing.
filterMaybe :: (a -> Bool) -> Maybe a -> Maybe a
filterMaybe p m@(Just x)
  | p x = m
filterMaybe p _
  = Nothing

-- Replace nth element of a list with a given item.
replace :: Int -> a -> [a] -> [a]
replace 0 p (c : cs)
  = p : cs
replace _ p []
  = []
replace n p (c : cs)
  = c : replace (n - 1) p cs

-- Returns the rows of a given board.
rows :: Board -> [[Cell]]
rows (cs , n)
  = rows' cs
  where
    rows' []
      = []
    rows' cs
      = r : rows' rs
      where
        (r, rs) = splitAt n cs

-- Returns the columns of a given board.
cols :: Board -> [[Cell]]
cols
  = transpose . rows

-- Returns the diagonals of a given board.
diags :: Board -> [[Cell]]
diags (cs, n)
  = map (map (cs !!)) [[k * (n + 1) | k <- [0 .. n - 1]],
                      [k * (n - 1) | k <- [1 .. n]]]

-------------------------------------------------------------------

gameOver :: Board -> Bool
gameOver bd@(cels, int) = row || column || diag
           where row    = or [True|x<-rows bd, 
                          nub x == [Taken X] || nub x ==[Taken O]]
                 column = or [True|x<-cols bd, 
                          nub x == [Taken X] || nub x ==[Taken O]]
                 diag   = or [True|x<-diags bd, 
                          nub x == [Taken X] || nub x ==[Taken O]]
-------------------------------------------------------------------

--
-- Moves must be of the form "row col" where row and col are integers
-- separated by whitespace. Bounds checking happens in tryMove, not here.
--
parsePosition :: String -> Maybe Position
parsePosition "" = Nothing 
parsePosition str 
        | Nothing `elem` check_int || length check_int /= 2 
                               = Nothing 
        | otherwise            = Just (fromJust x,fromJust (head xs))
        where check_int@(x:xs) = [readMaybe x :: Maybe Int | x<- words str]


tryMove :: Player -> Position -> Board -> Maybe Board
tryMove player (i,j) bd@(cels,int)
        | i >= int || j >= int || x /= Empty = Nothing 
        | otherwise                          = Just (new_cel, int) 
        where (x:xs)                         = drop pos cels
              pos                            = i*int+j
              new_cel                        = replace pos (Taken player) cels 

--check if all spots are occupied
isFull :: Board -> Bool
isFull bd@(cels, int) = and [x/=Empty| x<-cels]
-------------------------------------------------------------------
-- I/O Functions

prettyPrint :: Board -> IO ()
prettyPrint bd = putStrLn (concat new_bd) 
              --list of rows in the board
        where row_l   = rows bd
              -- every elem of every sublist undergoes the map func 
              row_l'  = map (map (\x -> if x == Empty then "-" 
                        else let Taken player = x in show player)) row_l
              new_row = map concat (map (" " `intersperse`) row_l')
              new_bd  = intersperse "\n" new_row
              
-- The following reflect the suggested structure, but you can manage the game
-- in any way you see fit.

-- Repeatedly read a target board position and invoke tryMove until
-- the move is successful (Just ...).

takeTurn :: Board -> Player -> IO Board
takeTurn bd player 
      = do 
           putStrLn "please enter a position - x y , if you do not want to play, press N"
           input  <- getLine 
           if input == "N"
               then do 
                    putStrLn ("Nothing is done, goodbye")
                    return bd
           else do
              let checked_pos = parsePosition input
              case checked_pos of 
                 Nothing -> do
                   putStrLn ("Oops, wrong position, try again")
                   takeTurn bd player 

                 Just _  -> do
                   let input_pos  = fromJust checked_pos
                   let check_move = tryMove player input_pos bd

                   case check_move of
                      Nothing -> do
                        putStrLn "Oops, invalid move, try again"
                        takeTurn bd player 

                      Just _  -> do
                        let new_bd = fromJust check_move
                        return new_bd

            

-- Manage a game by repeatedly: 
-- 1. printing the current board, 
-- 2. using takeTurn to return a modified board, 
-- 3. checking if the game is over, printing the board and a suitable congratulatory message to the winner
-- if so.

playGame :: Board -> Player -> IO ()
playGame bd player 
  = do 
       putStrLn "\n"
       prettyPrint bd
       new_bd <- takeTurn bd player
       let player' = show player

       if gameOver new_bd 
          then do
              putStrLn "\n"
              prettyPrint new_bd
              putStrLn ("Congratulations!" ++ player' ++ " win!")
        
       else 
          if isFull new_bd
             then putStrLn "no winner, next game"
          else 
             case player of 
                O -> playGame new_bd X
                X -> playGame new_bd O


-- Print a welcome message, read the board dimension, invoke playGame and
-- exit with a suitable message.
main :: IO ()
main
  = do
       putStrLn ("Welcome to the TicTacToe game")

       putStrLn ("please give a dimension of the board: ")
       input1   <- getLine 
       let n    = fromJust (readMaybe input1 :: Maybe Int)
       
       let cels = replicate (n^2) Empty
       playGame (cels,n) X

       putStrLn "Wish you have fun and lets do it again"

       return()


       
-------------------------------------------------------------------
testBoard1, testBoard2, testBoard3 :: Board

testBoard1
  = ([Taken O,Taken X,Empty,Taken O,
      Taken O,Empty,Taken X,Taken X,
      Taken O,Empty,Empty,Taken X,
      Taken O,Taken X,Empty,Empty],
      4)

testBoard2
  = ([Taken X,Empty,
      Empty,Empty],
      2)

testBoard3
  = ([Taken O,Taken X,Empty,Taken O,Taken X,
      Taken O,Empty,Taken X,Taken X,Empty,
      Empty,Empty,Taken X,Taken O,Taken O,
      Taken O,Taken X,Empty,Empty,Taken X,
      Taken X,Empty,Taken O,Empty,Empty],
      5)

testBoard4 :: ([Cell], Int)
testBoard4
  = ([Empty,Taken O,Empty,Empty,Taken O,Taken O,Taken X,Taken X,
      Taken X,Empty,Taken X,Taken X,Empty,Taken X,Taken O,Taken X,
      Empty,Empty,Taken X,Taken O,Taken O,Taken O,Taken X,Taken X,
      Taken O,Taken O,Empty,Empty,Taken O,Taken X,Taken O,Taken O,
      Taken X,Empty,Empty,Empty,Empty,Taken O,Taken O,Taken X],
      8)

testBoard5 :: ([Cell], Int)
testBoard5
  = ([Taken X,Taken O,Taken O,
      Empty,Taken X,Taken O,
      Taken O,Taken X,Taken X],
      3)

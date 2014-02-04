-- Basics
-- {{{
--    Setup/formatting
--    [[[

import Data.List

--      Stuff about the whole board
--      (((

currentBoard :: [[Int]]
currentBoard = [[0,0,0], -- We use balanced ternary to represent the board
                [0,0,0], -- 1 == x, -1 == o, 0 == open space
                [0,0,0]]

reverseBoard :: [[Int]] -> [[Int]]
reverseBoard = map (map (* (-1)))

--      note: the following three functions are all just parts of lineswith

hLinesWith :: Int -> [[Int]] -> Int -- horizontal line
hLinesWith n board = length [ x | x <- [0,1,2], sum (board !! x) == n ]

vLinesWith :: Int -> [[Int]] -> Int -- vertical lines
vLinesWith n board = hLinesWith n (transpose board)

dLinesWith :: Int -> [[Int]] -> Int -- diagonal lines
dLinesWith n board = length [ x | x <- [0,2], head board !! (2 - x) + board !! 1 !! 1 + board !! 2 !! x == n ]

linesWith :: Int -> [[Int]] -> Int -- Calculate the lines with a sum of n on a board
linesWith n board = hLinesWith n board + vLinesWith n board + dLinesWith n board

--      )))
--      stuff about subsections of the board
--      (((

isPlayed :: Int -> Int -> [[Int]] -> Bool -- is a square occupied?
isPlayed x y board = (board !! y !! x) /= 0

numSquaresPlayed :: [[Int]] -> Int -- how many squares are played?
numSquaresPlayed board = length (filter (/= 0) (concat board))

getOCoords :: [[Int]] -> (Int, Int) -- where's the first space with an O?
getOCoords board = head [ (x, y) | x <- [0,1,2], y <- [0,1,2], board !! y !! x == -1 ]

sumOCoords :: [[Int]] -> Int -- what's the sum of all the o coordinates?
sumOCoords board = sum (map (uncurry (+)) [ (x, y) | x <- [0,1,2], y <- [0,1,2], board !! y !! x == (-1) ])

--      )))
--    ]]]
--    Playing
--    [[[

replaceInList :: a -> Int -> [a] -> [a] -- change exactly one item in a list
replaceInList val index list = fst (splitAt index list) ++ [val] ++ drop 1 (snd $ splitAt index list)

play :: Int -> Int -> [[Int]] -> [[Int]] -- put an X on a space
play x y board = replaceInList (replaceInList 1 x (board !! y)) y board

opponentPlay :: Int -> Int -> [[Int]] -> [[Int]] -- put an O on a space
opponentPlay x y board = replaceInList (replaceInList (-1) x (board !! y)) y board

--    ]]]
-- }}}
-- Checking for conditions
-- {{{
--    Two-in-a-row
--    [[[

canWin ::  [[Int]] -> Bool
canWin board
  | linesWith 2 board > 0 = True
  | otherwise             = False

opponentCanWin ::  [[Int]] -> Bool
opponentCanWin board = canWin (reverseBoard board)

--    ]]]
--    Forcing a win
--    [[[

canForceWin :: [[Int]] -> Bool
canForceWin board
  | not (null [ (x,y) | x <- [0,1,2], y <- [0,1,2], linesWith 2 (play x y board) > 1 ]) = True
  | otherwise                                                                           = False

opponentCanForceWin :: [[Int]] -> Bool
opponentCanForceWin board = canForceWin(reverseBoard board)

--    ]]]
--    It's the first move
--    [[[

firstMove :: [[Int]] -> Bool
firstMove board
  | numSquaresPlayed board == 0  =  True
  | otherwise                    =  False

--    ]]]
--    It's the second move
--    [[[

secondMove :: [[Int]] -> Bool
secondMove board
  | numSquaresPlayed board == 1  =  True
  | otherwise                    =  False

--    ]]]
--    It's the third move
--    [[[

thirdMove :: [[Int]] -> Bool
thirdMove board
  | numSquaresPlayed board == 2  =  True
  | otherwise                    =  False

--    ]]]
--    It's the fourth move
--    [[[

fourthMove :: [[Int]] -> Bool
fourthMove board
  | numSquaresPlayed board == 3  =  True
  | otherwise                    =  False

--    ]]]
--    It's a tie
--    [[[

isTie :: [[Int]] -> Bool
isTie board = numSquaresPlayed board == 9

--    ]]]
--    We won
--    [[[

isWin :: [[Int]] -> Bool
isWin board = linesWith 3 board > 0

--    ]]]
-- }}}
-- Making moves
-- {{{
--    Playing a winning move
--    [[[

winningCoords :: [[Int]] -> (Int, Int) -- get the coordinates to play and win
winningCoords board = head [ (x, y) | x <- [0,1,2], y <- [0,1,2], linesWith 3 (play x y board) > 0, board !! y !! x == 0 ]

playWin :: [[Int]] -> [[Int]]
playWin board = uncurry play (winningCoords board) board

--    ]]]
--    Blocking a winning move
--    [[[

blockWin :: [[Int]] -> [[Int]]
blockWin board = reverseBoard (uncurry opponentPlay (winningCoords (reverseBoard board)) (reverseBoard board))

--    ]]]
--    Forcing a win
--    [[[

forceWinCoords :: [[Int]] -> (Int, Int) -- get the coordinates to force a win
forceWinCoords board = head [ (x, y) | x <- [0,1,2], y <- [0,1,2], linesWith 2 (play x y board) > 1, board !! y !! x == 0 ]

forceWin :: [[Int]] -> [[Int]]
forceWin board = uncurry play (forceWinCoords board) board

--    ]]]
--    Blocking a forced win
--    [[[

blockForceWin :: [[Int]] -> [[Int]]
blockForceWin board = reverseBoard (uncurry opponentPlay (forceWinCoords (reverseBoard board)) (reverseBoard board))

--    ]]]
--    For all the "making the nth move" functions, the source is
--    http://xkcd.com/832
--
--    Making the first move
--    [[[

playFirstMove :: [[Int]] -> [[Int]]
playFirstMove = play 0 0

--    ]]]
--    Making the second move
--    [[[

playSecondMove :: [[Int]] -> [[Int]]
playSecondMove board
  | board !! 1 !! 1 == 0 = play 1 1 board
  | otherwise            = play 0 0 board

--    ]]]
--    Making the third move
--    [[[

playThirdMove :: [[Int]] -> [[Int]]
playThirdMove board
  | getOCoords board == (1,1)                                       =  play 2 2 board
  | (fst (getOCoords board) == 1) || (snd (getOCoords board) == 1)  =  play 1 1 board
  | getOCoords board /= (2,0)                                       =  play 2 0 board
  | otherwise                                                       =  play 0 2 board

--    ]]]
--    Making the fourth move
--    [[[

fourthMoveEvenCoords :: [[Int]] -> (Int, Int) -- coordinates if both O's are orthogonal from the center
fourthMoveEvenCoords board = head [ (x,y) | x <- [0,1,2], y <- [0,1,2], sum (board !! y) == 1, sum (transpose board !! x) == 1 ]

fourthMoveOddCoords :: [[Int]] -> [(Int, Int)] -- coordinates for one ortho, one diagonal
fourthMoveOddCoords board = [ (x,y) | x <- [0,2], y <- [0,2], board !! (2 - y) !! (2 - x) == -1 ]

playFourthMove :: [[Int]] -> [[Int]]
playFourthMove board
  -- If both O's are across from one another, take an ortho space
  | sumOCoords board == 4 && head board !! 1 == 1                  =  play 0 1 board
  | sumOCoords board == 4                                          =  play 1 0 board
  -- If they take adjacent orthos, ply in between
  | sumOCoords board `mod` 2 == 0                                  =  uncurry play (fourthMoveEvenCoords board) board
  -- If there's an ortho and a diagonal, block the diagonal
  | not (null (fourthMoveOddCoords board))                         =  uncurry play (head (fourthMoveOddCoords board)) board
  | otherwise                                                      =  randomPlay board



--    ]]]
--    If the game is essentially tied, just play randomly
--
--    Random Play
--    [[[

randomCoords :: [[Int]] -> (Int,Int)
randomCoords board = head [ (x, y) | x <- [0,1,2], y <- [0,1,2], board !! y !! x == 0 ]

randomPlay :: [[Int]] -> [[Int]]
randomPlay board = uncurry play (randomCoords board) board

-- }}}

-- This is our big logic hub.  It tests the board for conditions and plays
-- accordingly

makeMove :: [[Int]] -> [[Int]]
makeMove board
  | canWin board              = playWin board
  | opponentCanWin board      = blockWin board
  | canForceWin board         = forceWin board
  | firstMove board           = playFirstMove board
  | secondMove board          = playSecondMove board
  | thirdMove board           = playThirdMove board
  | fourthMove board          = playFourthMove board
  | opponentCanForceWin board = blockForceWin board
  | otherwise                 = randomPlay board

-- Print things nicely
--
-- first we convert the board to X's and O's, and then we print it
--
-- {{{

printArray :: [String] -> IO ()
printArray board = putStrLn . unlines . map (unwords . map show) $ board

convertNums :: Int -> Char
convertNums a
  | a == 0    = '.'
  | a == 1    = 'X'
  | a == (-1) = 'O'

prettifyBoard :: [[Int]] -> IO ()
prettifyBoard board = printArray (map (map convertNums) board)

-- }}}
-- Tell us if the game is over
-- {{{

declareTie :: [[Int]] -> IO ()
declareTie board = do
  putStrLn "It's a tie"
  prettifyBoard board

declareWin :: [[Int]] -> IO ()
declareWin board = do
  putStrLn "X wins!"
  prettifyBoard board

-- }}}
-- Functions for actually playing the game against a person
-- {{{

changeTurn :: [[Int]] -> Int -> IO ()
changeTurn board turnVal
  | isWin board   =  declareWin board
  | isTie board   =  declareTie board
  | turnVal == 1  =  ourTurn board
  | otherwise     =  opponentTurn board

opponentTurn :: [[Int]] -> IO ()
opponentTurn board = do
  prettifyBoard board
  putStrLn "What is the X coordinate of the move you want to play?"
  xCoord <- getLine
  putStrLn "What is the Y coordinate of the move you want to play?"
  yCoord <- getLine
  if isPlayed (read xCoord) (read yCoord) board
  then do
    putStrLn "There's already a peice there"
    opponentTurn board
  else
    changeTurn (opponentPlay (read xCoord) (read yCoord) board) 1

ourTurn :: [[Int]] -> IO ()
ourTurn board = changeTurn (makeMove board) 0

initializeGame :: [[Int]] -> IO ()
initializeGame board = do
  putStrLn "Type 0 if you would like to go first, or 1 to go second"
  goFirst <- getLine
  if goFirst == "0"
    then
      opponentTurn board
    else
      ourTurn board

-- }}}
-- now just start the game on a blank board

main :: IO()
main = initializeGame currentBoard

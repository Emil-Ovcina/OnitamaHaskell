module Lib (
 generateRandom,
 isValid,
 hasWinningStrategy
 ) where

import System.IO
import Control.Monad
import System.Random
import Data.Maybe
import Text.Read (readMaybe)
import Data.List

import Debug.Trace

-- available cards in the game
cobra = Card "Cobra" [(0, -1), (-1, 1), (1, 1)]
rooster = Card "Rooster" [(0, 1),(1, 1),(0, -1),(-1, -1)]
monkey = Card "Monkey" [(-1, -1),(-1, 1),(1, 1),(1, -1)]
tiger = Card "Tiger" [(2, 0), (-1, 0)]
rabbit = Card "Rabbit" [(-1, -1), (0, 2), (1, 1)]
cardList = [cobra, rooster, monkey, tiger, rabbit]
nameOfCardList = ["Cobra", "Rooster", "Monkey", "Tiger", "Rabbit"]

-- data type for a card
data Card = Card {
    name :: String, 
    moves :: [(Int, Int)]
}

-- data type for the board
data Board = Board {
    getBoard :: [String],
    piecesA :: [(Int, Int)],
    piecesB :: [(Int, Int)],
    turn :: Int, -- 1 -> a ; 2 -> b
    erno :: Int -- line of where the error occurs
}

-- checks if inital board state is read correctly
checkBoard :: String -> Bool
checkBoard s = bo 
    where
        f = readMaybe s :: Maybe ([String], [(Int, Int)], [(Int, Int)])
        bo = if f == Nothing then False else True

-- Creates a board data type from the first input line in the file
createBoard :: String -> Board
createBoard x = if checkBoard x then 
  createBoard' (read x :: ([String], [(Int, Int)], [(Int, Int)]))
  else
    Board ([]) ([]) ([]) (1) (0)

createBoard' :: ([String], [(Int, Int)], [(Int, Int)]) -> Board
createBoard' x = Board (getCards x) (getPiecesA x) (getPiecesB x) 1 0

-- get the cards in the game
getCards :: ([String], [(Int, Int)], [(Int, Int)]) -> [String]
getCards (x, _ , _) = x

-- get player a pieces
getPiecesA :: ([String], [(Int, Int)], [(Int, Int)]) -> [(Int, Int)]
getPiecesA (_, x, _) = x

-- get player b pieces
getPiecesB :: ([String], [(Int, Int)], [(Int, Int)]) -> [(Int, Int)]
getPiecesB (_, _, x) = x

-- checks for lexigraphical order
checkLexi ::  (Ord a) => [a] -> Bool
checkLexi a
    | (sort $ take 2 a) == (take 2 a) && (sort $ take 2 $ drop 2 a) == (take 2 $ drop 2 a ) = True
    | otherwise = False

-- makes sure that the pieces dont move outside the board
checkCoords :: [(Int, Int)] -> Bool
checkCoords [] = True
checkCoords ((x, y):xs)
    | x > 4 || x < 0 || y > 4 || y < 0 = False
    | otherwise = checkCoords xs

-- checks for overlap between the pieces in the inital board state
checkOverlap :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
checkOverlap xs ys = filter (\x -> x `elem` ys) xs 

-- used to check for duplicates
hasDuplicates :: (Ord a) => [a] -> Bool
hasDuplicates xs = length (nub xs) /= length xs

isCardsAvailable :: [String] -> Bool
isCardsAvailable [] = True
isCardsAvailable (x:xs) = if not $ x `elem` nameOfCardList then False else isCardsAvailable xs

-- checks if the inital boardstate is valid
  -- test if way of the shrine
startingPosition :: Board -> Bool
startingPosition board@(Board cards pA pB _ _)
    | (length cards) /= 5 = False
    | not $ isCardsAvailable cards = False
    | not $ checkLexi cards = False
    | not $ checkCoords pA = False
    | not $ checkCoords pB = False
    | hasDuplicates pA || hasDuplicates pB = False
    | (length $ checkOverlap pA pB) > 0 = False
    | length pA == 0 && length pB == 0 = False
    | length pA > 5 && length pB > 5 = False
    | otherwise = True

-- returns card from string
getCard :: String -> Card
getCard s
        | (length list) >= 1 = list!!0
        | otherwise = Card "" []
          where list = (filter (\x -> name x == s) cardList)

-- gets the card from a move
getNameFromMove :: ((Int, Int), (Int, Int), String) -> String
getNameFromMove ((_,_),(_,_), s) = s

-- parses a string to a valid move, returns error if move is wrongly represented
getMove :: String -> ((Int, Int), (Int, Int), String)
getMove s = fromJust (readMaybe s :: Maybe ((Int, Int), (Int, Int), String))

-- gets move source
getMoveSrc :: ((Int, Int), (Int, Int), String) -> (Int, Int)
getMoveSrc ((x, y),(_,_), _) = (x, y)

-- gets move destiantion
getMoveDest :: ((Int, Int), (Int, Int), String) -> (Int, Int)
getMoveDest ((_, _),(x,y), _) = (x, y)

-- switches the used card with card on the table
useCard :: String -> [String] -> [String]
useCard card ls = insertAt lastCard newCI ((remove card $ take 4 ls) ++ [(ls!!newCI)])
          where 
            newCI = fromJust (elemIndex card ls)
            lastCard = last ls

-- Removes element in list
remove :: (Eq a) => a -> [a] -> [a]
remove element list = filter (\e -> e/=element) list

-- inserts element in list at index
insertAt :: a -> Int -> [a] -> [a]
insertAt newElement _ [] = [newElement]
insertAt newElement i (a:as)
  | i <= 0 = newElement:a:as
  | otherwise = a : insertAt newElement (i - 1) as

-- mirrors the whole board
mirrorBoard :: Board -> Board
mirrorBoard b = Board (getBoard b) (pa) (pb) (turn b) (erno b)
     where
         pa = mirrorPieces $ piecesA b
         pb = mirrorPieces $ piecesB b 

-- mirrors one list of pieces 
mirrorPieces :: [(Int, Int)] -> [(Int, Int)]
mirrorPieces [] = []
mirrorPieces ((x,y):xs) = [(4-x, 4-y)] ++ mirrorPieces xs

-- mirror cards 
mirrorCards :: [String] -> [String]
mirrorCards list = newList
      where 
        firstTwo = take 2 list 
        secondTwo = take 2 $ drop 2 list
        lastCard = last list
        newList = secondTwo ++ firstTwo ++ [lastCard]

-- checks if move input is valid
checkMove :: String -> Bool
checkMove s = bo 
    where
        f = readMaybe s :: Maybe ((Int, Int), (Int, Int), String)
        bo = if f == Nothing then False else True

-- recursive method that calls all the moves after the initial board state
makeMoves :: Board -> [String] -> Board
makeMoves b [] = b
makeMoves b (s:ls) = if checkMove s -- Correct format
  then 
        makeMoves (movePiece b s) ls
    else 
      throwErrorGame b 0

-- moves the piece, mirrors the board and checks every board state. 
movePiece :: Board -> String -> Board
movePiece b m = if (not bool2) && (isLegalMove src dest card)
  then
      if bool then
            if turn b == 1 then 
                checkGame $ mirrorBoard $ Board (useCard (getNameFromMove move) (getBoard b)) (doTheMove (piecesA b) src dest) (takeOpponentPiece b dest) 2 ((erno b) + 1)
              else
                checkGame $ mirrorBoard $ Board (useCard (getNameFromMove move) (getBoard b)) (takeOpponentPiece b dest) (doTheMove (piecesB b) src dest) 1 ((erno b) + 1)
      else throwErrorGame b 0
  else throwErrorGame b 0
       where
         move = getMove m
         card = getCard $ getNameFromMove move
         src = getMoveSrc move
         bool = isPlayersCard b card
         bool2 = ((name card) == "") && ((moves card) == [])
         dest = getMoveDest move

-- all pieces -> src -> destination -> new pieces
doTheMove :: [(Int, Int)] -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
doTheMove pieces src dest 
        | not bool = insertAt dest index (remove src pieces)
        | otherwise = [(0, -10)]
        where
          index = fromJust (findIndex (== src) pieces)
          bool = isNothing (findIndex (== src) pieces)

-- board -> dest from playing move -> new list of opponent pieces 
takeOpponentPiece :: Board -> (Int, Int) -> [(Int, Int)]
takeOpponentPiece b dest 
        | bool && (index == 0) = [] -- king has been taken - game ends
        | bool = remove dest pieces 
        | otherwise = pieces
          where
            pieces = if (turn b) == 1 then piecesB b else piecesA b 
            bool = not $ isNothing (findIndex (== dest) (pieces))
            index = fromJust (findIndex (== dest) (pieces)) 

-- src -> dest -> card used -> possible?
isLegalMove :: (Int, Int) -> (Int, Int) -> Card -> Bool
isLegalMove (mx, my) (x, y) c = (length (filter (\(cx, cy) -> (mx + cx, my + cy) == (x, y)) (moves c)) > 0)

-- checks if player has the played card
isPlayersCard :: Board -> Card -> Bool
isPlayersCard (Board cards _ _ t _) (Card cn _)
        | (t == 1) && (elem cn cards1) = True
        | (t == 2) && (elem cn cards2) = True
        | otherwise = False
          where 
            cards1 = take 2 cards
            cards2 = take 2 (drop 2 cards)

-- check the current board
checkGame :: Board -> Board
checkGame board@(Board cards pA pB _ _)
        | not $ checkCoords pA = throwErrorGame board 1
        | not $ checkCoords pB = throwErrorGame board 1
        | hasDuplicates pA || hasDuplicates pB = throwErrorGame board 1
        | checkWinning board == 1 = Board cards pA [] (-100) (0)
        | checkWinning board == 2 = Board cards [] pB (-200) (1)
        | otherwise = board

-- returns empty board if an error has occured
throwErrorGame :: Board -> Int -> Board
throwErrorGame b i = Board (getBoard b) ([]) ([]) (turn b) ((erno b) - i)

-- gets winning player, 1 if a, 2 if b
checkWinning :: Board -> Int
checkWinning b
        | length pA == 0 = 2
        | length pB == 0 = 1
        | turn b == 2 && kingA == (0, 2) = 1
        | turn b == 1 && kingB == (0, 2) = 2
        | otherwise = 0
          where
            pA = piecesA b
            kingA = (pA!!0)
            pB = piecesB b
            kingB = (pB!!0)

-- Generate a random starting board --([String], [(Int, Int)], [(Int, Int)]))
generateBoardState :: StdGen -> Board
generateBoardState gen = b
          where
            (cards, next) = split (gen)
            (pa, turnGen) = split next
            pieces = generatePieces pa
            b = createBoard $ "(" ++ show (lexiOrder $ generateCardList gen) ++ "," ++ (show $ fst pieces) ++ "," ++ (show $ snd pieces) ++ ")"

-- generates the two lists of player pieces
generatePieces :: StdGen -> ([(Int, Int)],[(Int, Int)])
generatePieces gen = (piecesA, piecesB)
          where
            (a, next) = split gen
            (aa, amount2) = randomR (1,5) next
            (ba, x4) = randomR (1,5) amount2
            list = (nub $ generatePieces' a)
            piecesA = take aa $ remove (4,2) list
            piecesB = take ba $ drop (aa+1) $ remove (0,2) list

-- helper function for generatePieces
generatePieces' :: StdGen -> [(Int, Int)]
generatePieces' gen = take 500 (zip x y)
          where 
            (xg, next) = split gen
            (yg, amount) = split next
            x = randomRs (0,4) xg
            y = randomRs (0,4) yg

-- shuffles the list of cards
generateCardList :: StdGen -> [String]
generateCardList gen = nub $ [(name $ cardList!!randomIndex)] ++ (generateCardList nextGen ) 
          where 
            (a, nextGen) = split gen
            (randomIndex, ft)  = (randomR (0,4) a)
-- lexographic ordering of list of strings - cards
lexiOrder :: [String] -> [String]
lexiOrder list = (sort $ take 2 list) ++ (sort $ take 2 $ drop 2 list) ++ [(list!!4)]

-- generates a move
generateMove :: StdGen -> Board -> ((Int, Int), (Int, Int), String)
generateMove gen board 
          | length listOfDests > 0 = (src, dest, cardString)
          | otherwise = generateMove gen5 board -- if no move can be generated with the chosen card and piece
           where
            (cardIndex, gen2) = randomR (0,1) gen
            (srcI, gen3) = randomR (0, (length $ pieces)-1) gen2
            (destGen, gen4) = split gen3
            (destI, gen5) = randomR (0, (length listOfDests)-1) gen4
            src = pieces!!srcI
            dest = listOfDests!!destI
            cardString = if turn board == 1 then (getBoard board)!!cardIndex else ((getBoard board)!!(cardIndex + 2))
            pieces = if turn board == 1 then piecesA board else piecesB board 
            card = getCard cardString
            listOfDests = generateRandomDests destGen board src (moves card) 50

-- generates a square for the piece to move to
generateRandomDests :: StdGen -> Board -> (Int, Int) -> [(Int, Int)] -> Int -> [(Int, Int)]
generateRandomDests _ _ _ _ 0 = []
generateRandomDests gen b src moves i = if bool then [(x,y)] ++ generateRandomDests nextGen b src moves (i-1) else generateRandomDests nextGen b src moves (i-1)
           where 
            (a, nextGen) = split gen
            (randomIndex, ft)  = (randomR (0,(length moves)-1) a)
            x = (fst src) + fst (moves!!randomIndex)
            y = (snd src) + snd (moves!!randomIndex)
            pieces = if turn b == 1 then piecesA b else piecesB b
            bool = checkCoords [(x,y)] && (not $ hasDuplicates ((pieces)++[(x,y)]))

-- recursevly generating moves and moving the pieces accordingly 
generateAllTheMoves :: StdGen -> Int -> Board -> String
generateAllTheMoves _ 0 b = ""
generateAllTheMoves gen index board = if turn board < 0 then "" else move ++ generateAllTheMoves nextGen (index - 1) newBoard
           where
            (nextGen, ng) = split gen
            move = (show $ generateMove ng board ) ++ "\n"
            newBoard = movePiece board move
            
-- returns the board in the desired format
readableBoard :: Board -> String
readableBoard board@(Board cards pa pb t _)
          | t == -100 = "("++ (show $ getCorrectCard cards 1) ++ "," ++ (show pb) ++ "," ++ (show pa) ++ ")"
          | t == -200 = "("++ (show $ getCorrectCard cards 2) ++ "," ++ (show pa) ++ "," ++ (show pb) ++ ")"
          | otherwise = "("++ (show $ getCorrectCard cards 2) ++ "," ++ (show pa) ++ "," ++ (show pb) ++ ")"

-- mirrors the card is needed
getCorrectCard :: [String] -> Int -> [String]
getCorrectCard s i
          | i == 1 = lexiOrder $ mirrorCards s
          | i == 2 = lexiOrder s

-- generate Random
generateRandom :: Int -> Int -> IO (String)
generateRandom seed n = do
    let (gboard, gmoves) = split (mkStdGen seed)
    let board = generateBoardState gboard
    let result = generateAllTheMoves gmoves n board
    let r = readableBoard board ++ "\n" ++ result
    return r

isValid :: FilePath -> IO (String)
isValid x = do 
    handle <- openFile x ReadMode
    content <- hGetContents handle
    let l = lines content
    let board = createBoard $ head l
  
    if piecesA board == [] && piecesB board == [] && getBoard board == [] && turn board == 1 && erno board == 0 -- used for coverage
      then do return "ParsingError" else do 
      if startingPosition board then do
        if length l == 1 then do return $ readableBoard board else do
            let b = (makeMoves (board) (drop 1 l)) 
            if getBoard b == ([""]) || getBoard b == ([]) || ((piecesA b) == [] && (piecesB b) == [])
              then do
                return ("NonValid " ++ ((drop 1 l)!!(erno b))) 
              else do
                if (erno b < 1) then do -- only used for coverage
                  return $ readableBoard b
                  else do return $ readableBoard b
        else do return "ParsingError"

hasWinningStrategy :: Int -> FilePath -> IO (String)
hasWinningStrategy _ _ = return "Not yet implemented"
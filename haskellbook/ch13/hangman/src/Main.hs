module Main where

import Control.Monad (forever)
import Data.Char     (toLower)
import Data.Maybe    (isJust)
import Data.List     (intersperse)
import System.Exit   (exitSuccess)
import System.Random (randomRIO)

newtype WordList = WordList [String] deriving (Eq, Show)

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList (filter predicate aw)
    where predicate w =
            let l = length (w :: String)
            in  l > minWordLength && l < maxWordLength

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO (0, length wl - 1)
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle = Puzzle String [Maybe Char] [Char]

instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    (intersperse ' ' $ fmap renderPuzzleChar discovered)
    ++ " Guessed so far: " ++ guessed

freshPuzzle :: String -> Puzzle
freshPuzzle s = Puzzle s (map (const Nothing) s) []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle s _ _) c = elem c s

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed) c = elem c guessed

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar (Just c) = c
renderPuzzleChar Nothing = '_'

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c = Puzzle word newFilledInSoFar (c:s)
  where
    newFilledInSoFar = zipWith (zipper c) word filledInSoFar
    zipper guessed wordChar guessedChar =
      if wordChar == guessed
      then Just wordChar
      else guessedChar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guessChar = do
  putStrLn $ "Your guess was: " ++ [guessChar]
  case (charInWord puzzle guessChar
      , alreadyGuessed puzzle guessChar) of
      (_, True) -> do
        putStrLn "You already guessed that\
                  \ character, pick something else!"
        return puzzle
      (True, _) -> do
        putStrLn "This character was in the word,\
                  \ filling in the word accordingly"
        return (fillInCharacter puzzle guessChar)
      (False, _) -> do
        putStrLn "This character wasn't in\
                  \ the word, try again"
        return (fillInCharacter puzzle guessChar)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed) =
  if (length guessed) > 7 then
    do putStrLn "You lose!"
       putStrLn $ "The word was: " ++ wordToGuess
       exitSuccess
  else return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) =
  if all isJust filledInSoFar then
    do putStrLn "You win!"
       exitSuccess
  else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin  puzzle
  putStrLn $ "current puzzle is: " ++ show puzzle
  putStrLn "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _   -> putStrLn "your guess must\
                    \ be a single character"

main :: IO ()
main = do
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle


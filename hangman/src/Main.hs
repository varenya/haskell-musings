module Main where
import           Debug.Trace   (trace)
import           Control.Monad (forever)
import           Data.Char     (toLower)
import           Data.List     (intersperse)
import           Data.Maybe    (isJust)
import           System.Exit   (exitSuccess)
import           System.Random (randomRIO)

data Puzzle =
  Puzzle String
         [Maybe Char]
         [Char]

newtype WordList =
  WordList [String]
  deriving (Eq, Show)

instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    (intersperse ' ' $ fmap renderPuzzleChar discovered) ++
    " Guessed so far " ++ guessed

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList (lines dict)

minWorldLength :: Int
minWorldLength = 5

maxWorldLength :: Int
maxWorldLength = 9

gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList (filter gameLength aw)
  where
    gameLength w =
      let l = length (w :: String)
      in l >= minWorldLength && l < maxWorldLength

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO (0, length wl - 1)
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

freshPuzzle :: String -> Puzzle
freshPuzzle inputStr = Puzzle inputStr (map (const Nothing) inputStr) []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle actualString _ _) char = elem char actualString

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing  = '_'
renderPuzzleChar (Just c) = c

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ enteredChar) char = elem char enteredChar

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) char =
  (Puzzle word newFilledInSoFar (char : s))
  where
    zipper guessed wordChar guessChar =
      if wordChar == guessed
        then Just wordChar
        else guessChar
    newFilledInSoFar = zipWith (zipper char) word filledInSoFar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was " ++ [guess]
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn
        "You already guessed that\
                  \ character , pick \
                  \something else!"
      return puzzle
    (True, _) -> do
      putStrLn
        "This character was in the \
                  \ word, fillilng in the word\
                  \ accordingly"
      return (fillInCharacter puzzle guess)
    (False, _) -> do
      putStrLn
        "This character wasn't in the word\
                  \ try again."
      return (fillInCharacter puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver puzzle@(Puzzle wordToGuess _ guessed) =
  if (length inCorrectGuesses) > 7
    then do
      putStrLn "You lose!"
      putStrLn $ "The word was " ++ wordToGuess
      exitSuccess
    else return ()
  where 
    inCorrectGuesses = (filter id . map (charInWord puzzle)) guessed

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) =
  if (all isJust filledInSoFar)
    then do
      putStrLn "You Win!"
      exitSuccess
    else return ()

runGame :: Puzzle -> IO ()
runGame puzzle =
  forever $ do
    gameOver puzzle
    gameWin puzzle
    putStrLn $ "Current puzzle is: " ++ show puzzle
    putStr "Guess a letter"
    guess <- getLine
    case guess of
      [c] -> handleGuess puzzle c >>= runGame
      _ ->
        putStrLn
          "Your guest must \
                        \ be a single character"

main :: IO ()
main = do
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle

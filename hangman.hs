import Data.List
import System.IO
import Control.Monad

alphabet = "abcdefghijklmnopqrstuvwxyz"
maxTries = 10

data Yolo = Yolo {swag :: Int, hype :: String} deriving (Show)

test :: Yolo
test = Yolo {swag = 5, hype = "hej"}

-- Word to guess -> Correct letters -> Used letters -> Num Guesses
playHangMan :: String -> [Char] -> [Char] -> Int -> IO ()
playHangMan word guessed used numGuessed = do
    putStrLn (getWordString word guessed)
    putStrLn $ "You have " ++ (show numGuessed) ++ " guesses left"
    if isGameWon word guessed then do
        putStrLn "Congrats!"
        putStrLn $ "The word was indeed " ++ word
        putStrLn "Goodbye"
    else when (isGameLost numGuessed) $ do 
        putStrLn "Haha, you lost!"
        putStrLn $ "The word was in fact: " ++ word
        
    putStrLn "Okay, give me a letter:"
    -- c <- getChar
    -- if isNewChar c word guessed used then do
    
        
    
isNewChar :: Char -> String -> [Char] -> [Char] -> Bool
isNewChar c word guessed used = not(elem c used) && not(elem c guessed) && elem c word 
    
getWordString :: String -> [Char] -> String
getWordString [] _ = []
getWordString (x:xs) y = let rest = getWordString xs y in
                            if elem x y
                            then [x, ' '] ++ rest
                            else "_ " ++ rest

isGameLost :: Int -> Bool
isGameLost tries = tries == maxTries

isGameWon :: String -> [Char] -> Bool
isGameWon [] _ = True
isGameWon (x:xs) y = elem x y && isGameWon xs y

main :: IO ()
main = do
    word <- getLine
    playHangMan word [] [] 0


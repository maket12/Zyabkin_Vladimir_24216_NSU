import System.Environment (getArgs)
import System.Exit (exitFailure)
import Data.List (elemIndex)
import Data.Maybe (fromJust)

-- Алфавит для шифрования
alph :: String
alph = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9']

-- Функция для шифрования одного символа
encryptChar :: Char -> Char -> Char
encryptChar char keyChar
    | char `elem` alph = alph !! ((charIndex + keyIndex) `mod` length alph)
    | otherwise = char
  where
    charIndex = fromJust (elemIndex char alph)
    keyIndex = fromJust (elemIndex keyChar alph)

-- Функция для дешифрования одного символа
decryptChar :: Char -> Char -> Char
decryptChar char keyChar
    | char `elem` alph = alph !! ((charIndex - keyIndex + length alph) `mod` length alph)
    | otherwise = char
  where
    charIndex = fromJust (elemIndex char alph)
    keyIndex = fromJust (elemIndex keyChar alph)

-- Общая функция для обработки текста
processText :: (Char -> Char -> Char) -> String -> String -> String
processText func text key = zipWith func text (cycle key)

main2 :: IO()
main2 = do
    args <- getArgs
    if length args /= 3
        then do
            putStrLn "Incorrect number of given elements! Check it out and try again!"
            exitFailure
        else do
            let mode = args !! 0
                key = args !! 1
                filepath = args !! 2
            case mode of
                "e" -> encode key filepath
                "d" -> decode key filepath
                _ -> give_error
  where
    give_error :: IO()
    give_error = do 
        putStrLn "Unknown mode! Action isn't available!"
        exitFailure

encode :: String -> FilePath -> IO()
encode key path = do
    content <- readFile path
    let encoded = processText encryptChar content key
        output_path = "output_encoded.txt"
    writeFile output_path encoded
    putStrLn ("The file was successfully encoded and saved at: " ++ output_path)

decode :: String -> FilePath -> IO()
decode key path = do
    content <- readFile path
    let decoded = processText decryptChar content key
        output_path = "output_decoded.txt"
    writeFile output_path decoded
    putStrLn ("The file was successfully decoded and saved at: " ++ output_path)

main :: IO()
main = do
    putStrLn "Goodies"
    main2  -- Вызов функции main2 для выполнения основного функционала

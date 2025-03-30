import System.Environment (getArgs)
import System.Exit (die)
import Data.Char (chr, ord)
import Control.Monad.State
import Control.Monad.Trans.Class (lift)

data BrainfuckState = BrainfuckState
  { memory :: [Int]
  , pointer :: Int
  , input :: String
  }

initialState :: BrainfuckState
initialState = BrainfuckState
  { memory = replicate 30000 0
  , pointer = 0
  , input = ""
  }

runBrainfuck :: String -> StateT BrainfuckState IO ()
runBrainfuck code = eval 0
  where
    eval i
      | i >= length code = return ()
      | otherwise = do
          let cmd = code !! i
          case cmd of
            '>' -> moveRight >> eval (i + 1)
            '<' -> moveLeft  >> eval (i + 1)
            '+' -> increment >> eval (i + 1)
            '-' -> decrement >> eval (i + 1)
            '.' -> outputChar >> eval (i + 1)
            ',' -> inputChar >> eval (i + 1)
            '[' -> do
              state <- get
              if safeGetAt (memory state) (pointer state) == 0
                then eval (jumpForward code (i + 1))
                else eval (i + 1)
            ']' -> do
              state <- get
              if safeGetAt (memory state) (pointer state) /= 0
                then eval (jumpBackward code (i - 1))
                else eval (i + 1)
            _ -> eval (i + 1)

jumpForward :: String -> Int -> Int
jumpForward code i = go 0 i
  where
    go 0 j | code !! j == ']' = j + 1
    go n j
      | j >= length code = error "No ]"
      | code !! j == '[' = go (n + 1) (j + 1)
      | code !! j == ']' = go (n - 1) (j + 1)
      | otherwise        = go n (j + 1)

jumpBackward code i = go 0 i
  where
    go 0 j | code !! j == '[' = j + 1
    go n j
      | j < 0 = error "No ["
      | code !! j == ']' = go (n + 1) (j - 1)
      | code !! j == '[' = go (n - 1) (j - 1)
      | otherwise        = go n (j - 1)


safeGetAt :: [Int] -> Int -> Int
safeGetAt mem i
  | i < 0 || i >= length mem = 0
  | otherwise = mem !! i

moveRight :: StateT BrainfuckState IO ()
moveRight = modify $ \s -> s { pointer = pointer s + 1 }

moveLeft :: StateT BrainfuckState IO ()
moveLeft = modify $ \s -> s { pointer = max 0 (pointer s - 1) }

increment :: StateT BrainfuckState IO ()
increment = modify $ \s ->
  let p = pointer s
      mem = memory s
      val = mem !! p + 1
      updated = take p mem ++ [val] ++ drop (p + 1) mem
  in s { memory = updated }

decrement :: StateT BrainfuckState IO ()
decrement = modify $ \s ->
  let p = pointer s
      mem = memory s
      val = mem !! p - 1
      updated = take p mem ++ [val] ++ drop (p + 1) mem
  in s { memory = updated }

outputChar :: StateT BrainfuckState IO ()
outputChar = do
  s <- get
  let val = safeGetAt (memory s) (pointer s)
  liftIO $ putChar (chr val)

inputChar :: StateT BrainfuckState IO ()
inputChar = do
  s <- get
  case input s of
    [] -> return ()
    (x:xs) ->
      let p = pointer s
          mem = memory s
          updated = take p mem ++ [ord x] ++ drop (p + 1) mem
      in put $ s { memory = updated, input = xs }

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> runProgram filename ""
    [filename, inputStr] -> runProgram filename inputStr
    _ -> die "Using: bf <file.bf> [input]"

runProgram :: FilePath -> String -> IO ()
runProgram filename inputStr = do
  code <- readFile filename
  let program = filter (`elem` "><+-.,[]") code
  putStrLn "[DEBUG] Programm code:"
  putStrLn program
  putStrLn "[DEBUG] Programm load"
  _ <- runStateT (runBrainfuck program) (initialState { input = inputStr })
  putStrLn "\n[DEBUG] Finished."




-- TESTS

-- "++++++++[>++++++++<-]>+." -> A
-- "+++++++++ [>++++++++<-] > ." -> H

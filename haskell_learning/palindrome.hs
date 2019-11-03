import Control.DeepSeq as Ctrl
import System.Environment as E
import System.IO
import Text.Printf as T

palin :: String -> Bool
palin [] = True
palin (c:[]) = True
palin (c:cs) | c /= (last cs) = False
             | otherwise      = palin (init cs)

interact_line :: (String -> String) -> IO ()
interact_line f = interact $ unlines . (map f) . lines

processLine s = s ++ " = " ++ (show $ palin s)

readFileComplex :: (String -> String) -> String -> IO String
readFileComplex f name = withFile name ReadMode loopBody where
  loopBody h = do
    done <- hIsEOF h
    if done
      then return ""
      else do
        line <- hGetLine h
        rest <- loopBody h
        return $ (f line) ++ "\n" ++ rest

readFileBuffer :: (String -> String) -> String -> IO String
readFileBuffer f name = withFile name ReadMode body where
  body h = do
    hSetBuffering h $ BlockBuffering (Just 4096)
    contents <- hGetContents h
    let result = (unlines . (map f) . lines) contents
    -- Be careful it is a trap ! The only way to force the evaluation is using `$!!`
    -- otherwise `result` will not be evaluated inside the `withFile` body and the handle will be closed
    return Ctrl.$!! result

readFileSimple :: (String -> String) -> String -> IO String
readFileSimple f name = do
  content <- readFile name
  return $ (unlines . (map f) . lines) content

readCmdArgs :: IO String
readCmdArgs = do
  words <-  E.getArgs
  let are_palins = map processLine words
  return (unlines are_palins)

main = do
  --interact_line processLine
  --result <- readFileComplex processLine "/tmp/haskell_learning/testinput"
  --result <- readFileSimple processLine "/tmp/haskell_learning/testinput"
  --result <- readFileBuffer processLine "/tmp/haskell_learning/testinput"
  result <- readCmdArgs
  T.printf result
  T.printf "you are done\n"

module PGFParse (parseWithPGF,findInAbsTrees) where
import PGF
import Data.Maybe
import Data.Bool
import Data.List

instance Show ParseOutput where
  show (ParseFailed i) = "Parse failed at " ++ show i
  show (TypeError tl) = "Type error"
  show (ParseOk t) = "Ok " ++ show t
  show ParseIncomplete = "Incomplete"

findInAbsTrees :: String -> [Expr] -> Bool
findInAbsTrees needle [] = False
findInAbsTrees needle (hd:tl) =
  findInAbsTree needle hd || findInAbsTrees needle tl
  
findInAbsTree :: String -> Expr -> Bool
findInAbsTree needle tree = isInfixOf needle (show tree)
  

parseLoop prel l postl pgf lang open =
  let
    parseRes = parseWithRecovery pgf lang (startCat pgf) open (Just 4) l
    parseOutput = fst parseRes
  in
    case parseOutput of
      (ParseOk ts) -> Right (prel,ts,postl)
      (ParseFailed i) ->
        let
          ws = words l
          newprel = if i > 1 then prel
                    else prel ++ " " ++ head ws
          newl = if i > 1 then unwords $ take (i - 1) $ ws
                 else unwords $ tail ws
          newpostl = if i > 1 then (unwords $ drop (i - 1) $ words l) ++ postl
                     else postl
        in
          parseLoop newprel newl newpostl pgf lang open
      _ -> Left parseOutput

parseWithPGF l pgf lang open =
  parseLoop "" l "" pgf lang open
  
readLoop pgf =
    do
      l <- getLine
      either (\p -> do putStrLn $ "Result: " ++ show p) (\(preB,trees,postB) -> putStrLn $ "Pre: " ++ preB ++ "\nTrees:\n" ++ (unlines $ map show trees) ++ "Post: " ++ postB ++ "\n") (parseWithPGF l pgf (head $ languages pgf) [])
      readLoop pgf
main =
    do
      pgf <- readPGF "Anna.pgf"
      readLoop pgf
    
    

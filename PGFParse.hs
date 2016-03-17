module PGFParse (parseWithPGF,parseOpenWithPGF,findInAbsTrees) where
import PGF
import PGF.Internal
import Data.Maybe
import Data.Bool
import Data.List
import Debug.Trace

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
findInAbsTree needle (EFun name) = name == (mkCId needle)
findInAbsTree needle (EApp t1 t2) = findInAbsTree needle t1 || findInAbsTree needle t2

matchStrings :: String -> String -> [(String,String)]
matchStrings s1 s2 =
  let
    matchWordLists [] _ = []
    matchWordLists _ [] = []
    matchWordLists (a:arest) (b:brest) = if a == b then matchWordLists arest brest else nub $ (a,b):(matchWordLists arest brest)
  in
    matchWordLists (words s1) (words s2)
                                                  
parseLoop :: [Char] -> String -> [Char] -> PGF -> Language -> [Type] -> Either ParseOutput ([Char], [Tree], [Char],[(String,String)])
parseLoop prel l postl pgf lang open =
  let
    parseRes = parseWithRecovery pgf lang (startCat pgf) open (Just 4) l
    parseOutput = fst parseRes
  in
    case parseOutput of
      (ParseOk ts) ->
        let
          linStrs = map (linearize pgf lang) ts
          context = concat $ map (matchStrings l) linStrs
          in
        trace "PARSEOK" $ Right (prel,ts,postl,context)
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
          trace (show i) $ parseLoop newprel newl newpostl pgf lang open
      _ -> Right (prel ++ " " ++ l ++ " " ++ postl, [] , [], []);

parseWithPGF l pgf lang =
  parseLoop "" l "" pgf lang []
  
parseOpenWithPGF l pgf lang open =
  parseLoop "" l "" pgf lang open

readLoop pgf lang = readLoopOpen pgf lang []
readLoopOpen pgf lang open =
    do
      l <- getLine
      either (\p -> do putStrLn $ "Result: " ++ show p) (\(preB,trees,postB,context) -> putStrLn $ "Pre: " ++ preB ++ "\nTrees:\n" ++ (unlines $ map show trees) ++ "Post: " ++ postB ++ "Context: " ++ show context ++ "\n") (parseOpenWithPGF l pgf lang open)
      readLoopOpen pgf lang open
main =
    do
      pgf <- readPGF "Anna.pgf"
--      readLoopOpen pgf (head $ languages pgf) [(fromJust $ readType "Placeholder")]
      readLoopOpen pgf (mkCId "AnnaEngQ") [(fromJust $ readType "Placeholder")]
    
    

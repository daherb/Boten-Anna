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
findInAbsTree needle (EMeta m) = needle == "?" ++ show m
matchStrings :: String -> String -> ([(String,String)],String)
matchStrings s1 s2 =
  let
    matchWordLists [] rest = ([],unwords rest)
    matchWordLists rest [] = ([],unwords rest)
    matchWordLists (a:arest) (b:brest) = if a == b then matchWordLists arest brest else let prev = (matchWordLists arest brest) in (nub $ ( (a,b):(fst prev)),snd prev)
  in
    matchWordLists (words s1) (words s2)

parseLoop :: [Char] -> String -> [Char] -> PGF -> Language -> [Type] -> ([Char], [Tree], [Char],[(String,String)])
parseLoop prel [] postl pgf lang open = 
  (prel, [] , postl, []);
parseLoop prel l postl pgf lang open =
  let
    parseRes = parseWithRecovery pgf lang (startCat pgf) open (Just 4) l
    parseOutput = fst parseRes
  in
    case parseOutput of
      (ParseOk ts) ->
        let
          linStrs = map (linearize pgf lang) ts
          (context,post) = matchStrings l $ head linStrs
          in
        trace "PARSEOK" $ (prel,ts,post ++ " " ++ postl,context)
      -- (ParseFailed i) ->
      --   let
      --     ws = words l
      --     newprel = if i > 1 then prel
      --               else prel ++ " " ++ head ws
      --     newl = if i > 1 then unwords $ take (i - 1) $ ws
      --            else unwords $ tail ws
      --     newpostl = if i > 1 then (unwords $ drop (i - 1) $ words l) ++ postl
      --                else postl
      --   in
      --     trace ("PARSE FAILED " ++ show i) $ parseLoop newprel newl newpostl pgf lang open
      (ParseIncomplete) ->
        let
          ws = words l
          newprel1 = prel ++ " " ++ head ws
          newl1 = unwords $ tail ws
          newpostl1 = postl
          newprel2 = prel
          newl2 = unwords $ init ws
          newpostl2 = last ws ++ " " ++ postl
          parsed1 = parseLoop newprel1 newl1 newpostl1 pgf lang open
          parsed2 = parseLoop newprel2 newl2 newpostl2 pgf lang open
        in
          -- When skipping in the back does not work, prefer the result of skipping in the front
          if (\(_,p,_,_) -> p == []) parsed2 then parsed1 else parsed2 
      _ -> trace "OTHER" $ (prel ++ " " ++ l ++ " " ++ postl, [] , [], []);

parseWithPGF :: String -> PGF -> Language -> ([Char], [Tree], [Char],[(String,String)])
parseWithPGF l pgf lang =
  parseLoop "" l "" pgf lang []

parseOpenWithPGF :: String -> PGF -> Language -> [Type] -> ([Char], [Tree], [Char],[(String,String)])
parseOpenWithPGF l pgf lang open =
  parseLoop "" l "" pgf lang open

readLoop pgf lang = readLoopOpen pgf lang []
readLoopOpen pgf lang open =
    do
      l <- getLine
      -- either (\p -> do putStrLn $ "Result: " ++ show p)
      (\(preB,trees,postB,context) -> putStrLn $ "Pre: " ++ preB ++ "\nTrees:\n" ++ (unlines $ map show trees) ++ "Post: " ++ postB ++ "Context: " ++ show context ++ "\n") (parseOpenWithPGF l pgf lang open)
      readLoopOpen pgf lang open
main =
    do
      pgf <- readPGF "Anna.pgf"
--      readLoopOpen pgf (head $ languages pgf) [(fromJust $ readType "Placeholder")]
      readLoopOpen pgf (mkCId "AnnaEngQ") [(fromJust $ readType "Placeholder")]
    
    

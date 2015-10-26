module PGFParse (parseWithPGF) where
import PGF
import Data.Maybe
import Control.Exception

handleOutputs (_,b) = b

parseLoop prel l pgf =
  let
    parseRes = parse_ pgf (head $ languages pgf) (startCat pgf) Nothing l
    brackets = snd parseRes
    cat = (\x -> case x of Bracket c _ _ _ _ _ -> c) brackets
    pos = (\x -> case x of ParseFailed i -> i - 1
                           _ -> length $ words l) $ fst parseRes
    rest = unwords $ drop pos $ words l  
  in
    if showCId cat == "_" then
        if l == "" then
            Nothing
        else
          let
            ws = words l
          in
            parseLoop (prel ++ " " ++ head ws) (unwords $ tail $ ws) pgf
    else
        if (showCId cat) /= (showType [] $ startCat pgf) then 
            Nothing
        else
            Just (prel,brackets,rest) --(maximum parses)

parseWithPGF l pgf =
  parseLoop "" l pgf
  
readLoop pgf =
    do
      l <- getLine
      let (preB,brackets,postB) = fromJust $ parseLoop "" l pgf
      catch  (putStrLn $ "Pre: " ++ preB ++ " Brackets: " ++ (showBracketedString $ brackets) ++ " Post: " ++ postB) (\e -> let s = show(e :: SomeException) in putStrLn $ "Panic: " ++ s)
      readLoop pgf
main =
    do
      pgf <- readPGF "Anna.pgf"
      readLoop pgf
      

module PGFParse (parseLoop) where
import PGF
import Data.Maybe
import Control.Exception
    
handleOutputs (_,b) = b
                      
parseLoop l pgf =
  let
    parses = (\p -> case p of (_ , b) -> b) $ parse_ pgf (head $ languages pgf) (startCat pgf) Nothing l
    cat = (\x -> case x of Bracket c _ _ _ _ _ -> c) parses
  in
    if showCId cat == "_" then
        if l == "" then
            Nothing
        else
            parseLoop (unwords $ tail $ words l) pgf
    else
        if (showCId cat) /= (showType [] $ startCat pgf) then 
            Nothing
        else
            Just parses --(maximum parses)

readLoop pgf =
    do
      l <- getLine
      catch (putStrLn $ showBracketedString $ fromJust $ parseLoop l pgf) (\e -> let s = show(e :: SomeException) in putStrLn $ "Panic: " ++ s)
      readLoop pgf
main =
    do
      pgf <- readPGF "partial.pgf"
      readLoop pgf
      

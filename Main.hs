import Network.SimpleIRC
import Data.Maybe
import PGFParse
import PGF
import Data.Char
import qualified Data.ByteString.Char8 as B

botnick = "annaaerinteenbot"
channelname = "#botenannatest"

pgf = readPGF "Anna.pgf"


sendResponse s m pre parsed post
  | findInBracketed "please" parsed && findInBracketed "de-op" parsed =
    let
      ws = words post
    in
      if head ws == "me" then
        sendCmd s (MMode (B.pack channelname) (B.pack "-o") (mNick m))
      else
        sendCmd s (MMode (B.pack channelname) (B.pack "-o") (Just $ B.pack $ head ws))
  | findInBracketed "please" parsed && findInBracketed "op" parsed =
    let
      ws = words post
    in
      if head ws == "me" then
        sendCmd s (MMode (B.pack channelname) (B.pack "+o") (mNick m))
      else
        sendCmd s (MMode (B.pack channelname) (B.pack "+o") (Just $ B.pack $ head ws))
  | findInBracketed "op" parsed || findInBracketed "deop" parsed =
      sendMsg s (fromJust $ mChan m) (B.pack "You have to be more polite if I should help you")
  | findInBracketed "anna" parsed || findInBracketed "annas" parsed =
      sendMsg s (fromJust $ mChan m) (B.pack "Are you talking about me?")
  | findInBracketed "bot" parsed || findInBracketed "bots" parsed =
      sendMsg s (fromJust $ mChan m) (B.pack "I am not a bot!")
  | (head $ words pre) == (botnick ++ ":") =
      sendMsg s (fromJust $ mChan m) (B.pack $ "What do you want to accomplish by saying: \"" ++ ( unwords $ tail $ words pre ) ++ "\"")
  | otherwise = return ()
onPrivMsg :: EventFunc
onPrivMsg s m =
  do
    mpgf <- pgf
    let text = B.unpack $ mMsg m
    case parseWithPGF (map toLower text) mpgf of
      Nothing -> sendResponse s m (map toLower text) [] ""
      Just (pre,parsed,post) -> sendResponse s m pre [parsed] post
--    putStrLn $ show m
                
onJoinMsg :: EventFunc
onJoinMsg s m =
    do
      putStr "Joinmsg: "
      putStrLn $ show m
      sendCmd s (MMode chan (B.pack "+o") (mNick m))
    where chan = B.pack channelname


main =
    do
      messages <- newIORef ([])
      let events    = [(Privmsg (onPrivMsg messages)), (Join (onJoinMsg messages))]
      let config    = IrcConfig {
          cAddr                = "irc.freenode.net",
          cPort                = 6667,
          cSecure              = False,
          cNick                = botnick,
          cPass                = Nothing, -- Optional server password
          cUsername            = botnick,
          cRealname            = "Boten Anna",
          cChannels            = [channelname],
          cEvents              = events,
          cPingTimeoutInterval = 35 * 10^(6::Int),
          cCTCPVersion         = "Boten-Anna " ++ version,
          cCTCPTime            = fmap (formatTime defaultTimeLocale "%c") getZonedTime
       }                               
      connect config False True


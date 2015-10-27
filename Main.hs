import Network.SimpleIRC
import Data.Maybe
import PGFParse
import PGF
import Data.Char
import qualified Data.ByteString.Char8 as B
import Data.IORef
import qualified Data.List
import Data.List.Unique
import Data.Time
import System.Locale

version = "0.2"
botnick = "annaaerinteenbot"
channelname = "#botenannatest"

pgf = readPGF "Anna.pgf"
  
sendResponse s m pre parsed post iomessages
  | findInBracketed "tell" parsed && (length ( words post ) > 2) =
     let
       to = head $ words post
       message = unwords $ tail $ words post
     in
       do
         messages <- readIORef iomessages
         let newMessages = (B.unpack (fromJust (mNick m)),to,message):messages
         writeIORef iomessages newMessages
         sendMsg s (fromJust $ mChan m) ( B.pack $ ( B.unpack (fromJust (mNick m) ) ) ++ ": I will transmit your message to " ++ to )
  | findInBracketed "please" parsed && findInBracketed "de-op" parsed && (length ( words post ) > 2) =
    let
      ws = words post
    in
      if head ws == "me" then
        sendCmd s (MMode (B.pack channelname) (B.pack "-o") (mNick m))
      else
        if head ws == botnick then
          do
            sendMsg s (fromJust $ mChan m) (B.pack "How dare you?")
            sendCmd s (MMode (B.pack channelname) (B.pack "-o") (mNick m))
        else
          sendCmd s (MMode (B.pack channelname) (B.pack "-o") (Just $ B.pack $ head ws))
  | findInBracketed "please" parsed && findInBracketed "op" parsed && (length ( words post ) > 2) =
    let
      ws = words post
    in
      if head ws == "me" then
        sendCmd s (MMode (B.pack channelname) (B.pack "+o") (mNick m))
      else
        sendCmd s (MMode (B.pack channelname) (B.pack "+o") (Just $ B.pack $ head ws))
  | findInBracketed "op" parsed || findInBracketed "deop" parsed =
      sendMsg s (fromJust $ mChan m) (B.pack "You have to be more polite if I should help you")
  | findInBracketed "Name" parsed =
      sendMsg s (fromJust $ mChan m) (B.pack "Are you talking about me?")
  | findInBracketed "Bot" parsed =
      sendMsg s (fromJust $ mChan m) (B.pack "I am not a bot!")
  | (length ( words pre ) >= 1) && (head $ words pre) == (botnick ++ ":") =
      sendMsg s (fromJust $ mChan m) (B.pack $ "What do you want to accomplish by saying: \"" ++ ( unwords $ tail $ words pre ) ++ "\"")
  | otherwise = return ()
onPrivMsg :: IORef ([(String,String,String)]) -> EventFunc
onPrivMsg messages s m =
  do
    mpgf <- pgf
    let text = B.unpack $ mMsg m
    case parseWithPGF (map toLower text) mpgf of
      Nothing -> sendResponse s m (map toLower text) [] "" messages
      Just (pre,parsed,post) -> sendResponse s m pre [parsed] post messages

printMessages :: [(String,String,String)] -> MIrc -> B.ByteString -> B.ByteString -> IO [String]
printMessages [] _ _ _ = do return []
printMessages ((from,to,message):ms) s nick chan = 
  if to == B.unpack nick then
    do
      sendMsg s chan $ B.pack ((B.unpack nick) ++ ": " ++ from ++ " wants me to tell you " ++ message)
      sendMsg s chan $ B.pack (from ++ ": Transmitted your message to " ++ (B.unpack nick))
      tos <- printMessages ms s nick chan
      return (to:tos)
   else
     do
       printMessages ms s nick chan
  
onJoinMsg :: IORef ([(String,String,String)]) -> EventFunc
onJoinMsg iomessages s m =
    do
      messages <- readIORef iomessages
      sendCmd s (MMode chan (B.pack "+o") (Just nick))
      transmitted <- printMessages messages s nick chan
      let filteredMessages = filter (\(from,to,msg) -> to /= B.unpack nick) messages
      
      writeIORef iomessages filteredMessages

    where chan = B.pack channelname
          nick = fromJust (mNick m)

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
          cPingTimeoutInterval = 150 * 10^(6::Int),
          cCTCPVersion         = "Boten-Anna " ++ version,
          cCTCPTime            = fmap (formatTime defaultTimeLocale "%c") getZonedTime
       }                               
      connect config False True


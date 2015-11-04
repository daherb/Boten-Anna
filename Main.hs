import Network.SimpleIRC
import Data.Maybe
import PGFParse
import PGF
import Data.Char
import qualified Data.ByteString.Char8 as B
import Data.IORef

version = "0.2"
botnick = "annaisnotabot"
channelname = "#botenannatest"

pgf = readPGF "Anna.pgf"

sendResponse :: MIrc -> IrcMessage -> String -> [Expr] -> String -> IORef [([Char], String, String)] -> IO ()
sendResponse s m pre parsed post iomessages
  | findInAbsTrees "tell" parsed && (length ( words post ) >= 2) =
     let
       to = head $ words post
       rcpt = if to == "me" then B.unpack nick else to
       message = unwords $ tail $ words post
     in
       do
         messages <- readIORef iomessages
         let newMessages = (B.unpack nick,rcpt,message):messages
         writeIORef iomessages newMessages
         sendMsg s chan ( B.pack $ ( B.unpack nick ) ++ ": I will transmit your message to " ++ if to == "me" then "yourself" else rcpt)
  | findInAbsTrees "please" parsed && findInAbsTrees "deop" parsed && (length ( words post ) == 1 ) =
    let
      ws = words post
    in
      if head ws == "me" then
        sendCmd s (MMode chan (B.pack "-o") (mNick m))
      else
        if head ws == botnick then
          do
            sendMsg s chan (B.pack "How dare you?")
            sendCmd s (MMode chan (B.pack "-o") (mNick m))
        else
          sendCmd s (MMode chan (B.pack "-o") (Just $ B.pack $ head ws))
  | findInAbsTrees "please" parsed && findInAbsTrees "op" parsed && (length ( words post ) == 1 ) =
    let
      ws = words post
    in
      if head ws == "me" then
        sendCmd s (MMode chan (B.pack "+o") (mNick m))
      else
        sendCmd s (MMode chan (B.pack "+o") (Just $ B.pack $ head ws))
  | findInAbsTrees "please" parsed && (findInAbsTrees "op" parsed || findInAbsTrees "deop" parsed ) && (length ( words post ) > 1 ) =
      sendMsg s chan (B.pack "You are a little bit verbose, aren't you?")
  | findInAbsTrees "op" parsed || findInAbsTrees "deop" parsed =
      sendMsg s chan (B.pack "You have to be more polite if I should help you")
  | findInAbsTrees "name" parsed =
      sendMsg s chan (B.pack "Are you talking about me?")
  | findInAbsTrees "bot" parsed =
      sendMsg s chan (B.pack "I am not a bot!")
  | (length ( words pre ) >= 1) && (head $ words pre) == (botnick ++ ":") =
      sendMsg s chan (B.pack $ "What do you want to accomplish by saying: \"" ++ ( unwords $ tail $ words pre ) ++ "\"")
  | otherwise = return ()
  where chan = if isJust (mChan m) then fromJust (mChan m) else B.pack ""
        nick = if isJust (mNick m) then fromJust (mNick m) else B.pack ""
onPrivMsg :: IORef ([(String,String,String)]) -> EventFunc
onPrivMsg iomessages s m =
  do
    mpgf <- pgf
    messages <- readIORef iomessages
    let text = B.unpack $ mMsg m
    transmitted <- printMessages messages s nick chan
    let filteredMessages = filter (\(from,to,msg) -> to /= B.unpack nick) messages
    writeIORef iomessages filteredMessages
    case parseWithPGF (map (\c -> if elem c ".,!?" then ' ' else toLower c) text) mpgf of
      Left res -> 
        putStrLn $ show res
      Right (pre,parsed,post) -> do
        putStrLn $ "Parse trees: " ++ (show parsed)
        sendResponse s m pre parsed post iomessages
  where chan = if isJust (mChan m) then fromJust (mChan m) else B.pack ""
        nick = if isJust (mNick m) then fromJust (mNick m) else B.pack ""

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

    where chan = if isJust (mChan m) then fromJust (mChan m) else (mMsg m)
          nick = if isJust (mNick m) then fromJust (mNick m) else B.pack ""

onInviteMsg :: EventFunc
onInviteMsg s m =
    do
      sendCmd s (MJoin (mMsg m) Nothing)
    where chan = if isJust (mChan m) then fromJust (mChan m) else (mMsg m)
          nick = if isJust (mNick m) then fromJust (mNick m) else B.pack ""

main :: IO (Either IOError MIrc)
main =
    do
      messages <- newIORef ([])
      let events    = [(Privmsg (onPrivMsg messages)), (Join (onJoinMsg messages)), (Invite onInviteMsg)]
      let config    = (mkDefaultConfig "" "") {
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
          cCTCPVersion         = "Boten-Anna " ++ version
--          cCTCPTime            = fmap (formatTime defaultTimeLocale "%c") getZonedTime
       }                               
      info <- connect config False True
      server <- let (Right l) = info in return l
      return info

import Network.SimpleIRC
import Data.Maybe
import PGFParse
import PGF
import Data.Char
import qualified Data.ByteString.Char8 as B
import Data.IORef
import Data.List
version = "0.3"
botnick = "annaisnotabot"
channelnames = ["#botenannatest","#botenannatest1"]

data Message = Envelop {
  channel :: String,
  from :: String,
  to :: String,
  message :: String };
  
pgf = readPGF "Anna.pgf"

normalize :: String -> String
normalize = map (\c -> if elem c ".,!?" then ' ' else toLower c)

sendResponse :: MIrc -> IrcMessage -> String -> [Expr] -> String -> IORef [Message] -> IO ()
sendResponse s m pre parsed post iomessages =
    do
      grammar <- pgf
      let response = linearize grammar (mkCId "EngR") $ head parsed
      return ()
  -- | isPrefixOf ( botnick ++ ":" ) pre && findInAbsTrees "tell" parsed && (length ( words post ) >= 2) =
  --    let
  --      to = head $ words post
  --      rcpt = if to == "me" then B.unpack nick else to
  --      message = unwords $ tail $ words post
  --    in
  --      do
  --        putStrLn "Case 1"
  --        messages <- readIORef iomessages
  --        let newMessages = (Envelop { channel = B.unpack chan, from = B.unpack nick, to = rcpt, message = message}):messages
  --        writeIORef iomessages newMessages
  --        sendMsg s chan ( B.pack $ ( B.unpack nick ) ++ ": I will transmit your message to " ++ if to == "me" then "yourself" else rcpt)
  -- | pre == ( botnick ++ ":" )&& findInAbsTrees "help" parsed &&  (length ( words post ) == 0) = -- (length ( words pre ) == 0) &&
  --     do
  --       sendMsg s chan ( B.pack $ ( B.unpack nick ) ++ ": ")
  -- | findInAbsTrees "ping" parsed && (length ( words post ) == 1 ) =
  --     let
  --         to = head $ words post
  --     in
  --       do
  --         sendMsg s chan ( B.pack $ ( if to == "me" then B.unpack nick else to ) ++ ": ping from " ++ ( if to == "me" then "yourself" else B.unpack nick) )
  -- | findInAbsTrees "please" parsed && findInAbsTrees "deop" parsed && (length ( words post ) == 1 ) =
  --   let
  --     ws = words post
  --   in
  --     if head ws == "me" then
  --       sendCmd s (MMode chan (B.pack "-o") (mNick m))
  --     else
  --       if head ws == botnick then
  --         do
  --           sendMsg s chan (B.pack "How dare you?")
  --           sendCmd s (MMode chan (B.pack "-o") (mNick m))
  --       else
  --         sendCmd s (MMode chan (B.pack "-o") (Just $ B.pack $ head ws))
  -- | findInAbsTrees "please" parsed && findInAbsTrees "op" parsed && (length ( words post ) == 1 ) =
  --   let
  --     ws = words post
  --   in
  --     if head ws == "me" then
  --       sendCmd s (MMode chan (B.pack "+o") (mNick m))
  --     else
  --       sendCmd s (MMode chan (B.pack "+o") (Just $ B.pack $ head ws))
  -- | findInAbsTrees "please" parsed && (findInAbsTrees "op" parsed || findInAbsTrees "deop" parsed ) && (length ( words post ) > 1 ) =
  --     sendMsg s chan (B.pack "You are a little bit verbose, aren't you?")
  -- | findInAbsTrees "op" parsed || findInAbsTrees "deop" parsed =
  --     sendMsg s chan (B.pack "You have to be more polite if I should help you")
  -- | findInAbsTrees "name" parsed =
  --     sendMsg s chan (B.pack "Are you talking about me?")
  -- | findInAbsTrees "bot" parsed =
  --     sendMsg s chan (B.pack "I am not a bot!")
  -- | (length ( words pre ) >= 1) && isPrefixOf botnick pre = --(head $ words pre) == (botnick ++ ":") =
  --     sendMsg s chan (B.pack $ "What do you want to accomplish by saying: \"" ++ ( unwords $ tail $ words pre ) ++ "\"")
  -- | otherwise = return ()
  where chan = if isJust (mChan m) then fromJust (mChan m) else B.pack ""
        nick = if isJust (mNick m) then fromJust (mNick m) else B.pack ""
        
onPrivMsg :: IORef ([Message]) -> IORef ([String]) -> EventFunc
onPrivMsg iomessages ionicks s m =
  do
    mpgf <- pgf
    messages <- readIORef iomessages
    let text = B.unpack $ mMsg m
    remaining <- printMessages messages s nick chan
    writeIORef iomessages remaining
    putStrLn ("###" ++ text)
    case parseWithPGF (normalize text) mpgf (mkCId "EngQ") [mkType [] (mkCId "Placeholder") []] of
      Left res -> do
        putStrLn $ show res
        sendResponse s m text [] "" iomessages
      Right (pre,parsed,post) -> do
        putStrLn $ "Pre: " ++ pre ++ " Parse trees: " ++ (show parsed) ++ " Post: " ++ post ++ " EOL"
        sendResponse s m pre parsed post iomessages
  where chan = if isJust (mChan m) then fromJust (mChan m) else B.pack ""
        nick = if isJust (mNick m) then fromJust (mNick m) else B.pack ""

printMessages :: [Message] -> MIrc -> B.ByteString -> B.ByteString -> IO [Message]
printMessages [] _ _ _ = do return []
printMessages ((msg@(Envelop {channel = c, from = f,to = t, message = m})):ms) s nick chan = 
  if t == (normalize $ B.unpack nick) && c == B.unpack chan then
    do
      sendMsg s chan $ B.pack ((B.unpack nick) ++ ": " ++ f ++ " wants me to tell you " ++ m)
      sendMsg s chan $ B.pack (f ++ ": Transmitted your message to " ++ (B.unpack nick))
      rest <- printMessages ms s nick chan
      return rest
   else
     do
       rest <- printMessages ms s nick chan
       return (msg:rest)
  
onJoinMsg :: IORef ([Message]) -> IORef ([String]) -> EventFunc
onJoinMsg iomessages ionicks s m =
  do
    messages <- readIORef iomessages
    nicks <- readIORef ionicks
    sendCmd s (MMode chan (B.pack "+o") (Just nick))
    remaining <- printMessages messages s nick chan
    writeIORef iomessages remaining
    writeIORef ionicks ((B.unpack nick):nicks)
    where chan = if isJust (mChan m) then fromJust (mChan m) else (mMsg m)
          nick = if isJust (mNick m) then fromJust (mNick m) else B.pack ""

onInviteMsg :: EventFunc
onInviteMsg s m =
    do
      sendCmd s (MJoin (mMsg m) Nothing)
    where chan = if isJust (mChan m) then fromJust (mChan m) else (mMsg m)
          nick = if isJust (mNick m) then fromJust (mNick m) else B.pack ""

onPartMsg :: IORef ([String]) -> EventFunc
onPartMsg ionicks s m =
  do
    nicks <- readIORef ionicks
    writeIORef ionicks (filter (\n -> n /= B.unpack nick ) nicks)
    where chan = if isJust (mChan m) then fromJust (mChan m) else (mMsg m)
          nick = if isJust (mNick m) then fromJust (mNick m) else B.pack ""
main :: IO (Either IOError MIrc)
main =
    do
      messages <- newIORef ([])
      nicks <- newIORef ([])
      let events    = [(Privmsg (onPrivMsg messages nicks)), (Join (onJoinMsg messages nicks)), (Invite onInviteMsg), (Part (onPartMsg nicks))]
      let config    = (mkDefaultConfig "" "") {
          cAddr                = "chat.freenode.net",
          cPort                = 6667,
          cSecure              = False,
          cNick                = botnick,
          cPass                = Nothing, -- Optional server password
          cUsername            = botnick,
          cRealname            = "Boten Anna",
          cChannels            = channelnames,
          cEvents              = events,
          cPingTimeoutInterval = 350 * 10^(6::Int),
          cCTCPVersion         = "Boten-Anna " ++ version
--          cCTCPTime            = fmap (formatTime defaultTimeLocale "%c") getZonedTime
       }                               
      info <- connect config False True
      server <- let (Right l) = info in return l
      return info

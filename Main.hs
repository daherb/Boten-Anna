import Network.SimpleIRC
import Data.Maybe
import PGFParse
import PGF
import Data.Char
import qualified Data.ByteString.Char8 as B
import Data.IORef
import Data.List
import qualified Data.Set as S
import System.IO
import Control.Monad

version = "0.3"
botNick = "annaisnotabot"
channelNames = ["#botenannatest","#botenannatest1"]
debugMsg = False
logging = True
logName = "boten-anna.log"

type NickList = ([(B.ByteString,S.Set B.ByteString)])

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

onPrivMsg :: IORef ([Message]) -> IORef NickList -> EventFunc
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
  
onJoinMsg :: IORef ([Message]) -> IORef NickList -> EventFunc
onJoinMsg iomessages ionicklist s m =
  do
    messages <- readIORef iomessages
    nickList <- readIORef ionicklist
    if nick == (B.pack botNick) then
      sendRaw s (B.pack "NAMES channel")
    else
      sendCmd s (MMode chan (B.pack "+o") (Just nick))
    remaining <- printMessages messages s nick chan
    writeIORef iomessages remaining
    let newNickList = map (\(c,ns) -> if c == chan then (c, S.insert nick ns) else (c,ns)) nickList
    writeIORef ionicklist newNickList
    putStrLn $ show newNickList
    where chan = if isJust (mChan m) then fromJust (mChan m) else (mMsg m)
          nick = if isJust (mNick m) then fromJust (mNick m) else B.pack ""
    

onInviteMsg :: IORef NickList -> EventFunc
onInviteMsg ionicklist s m =
    do
      sendCmd s (MJoin (mMsg m) Nothing)
      nickList <- readIORef ionicklist
      let newNickList = if not (isNothing $ find (\(c,_) -> c == chan) nickList) then ((chan, S.empty):nickList) else nickList
      writeIORef ionicklist newNickList
    where chan = if isJust (mChan m) then fromJust (mChan m) else (mMsg m)
          nick = if isJust (mNick m) then fromJust (mNick m) else B.pack ""

onPartMsg :: IORef NickList -> EventFunc 
onPartMsg ionicklist s m =
  do
    nickList <- readIORef ionicklist
    let newNickList = map (\(c,ns) -> if c == chan then (c, ns S.\\ (S.singleton nick)) else (c,ns)) nickList
    writeIORef ionicklist newNickList
    putStrLn $ show newNickList
    where chan = if isJust (mChan m) then fromJust (mChan m) else (mMsg m)
          nick = if isJust (mNick m) then fromJust (mNick m) else B.pack ""

-- >> :rajaniemi.freenode.net 353 annaisnotabot @ #botenannatest :annaisnotabot @daherb
onRawMsg :: IORef Handle -> IORef NickList -> EventFunc
onRawMsg iohandle ionicklist s m =
  do
    handle <- readIORef iohandle
    nickList <- readIORef ionicklist
    when logging $ hPutStrLn handle $ show m
    if ((B.isInfixOf . B.pack) " 353 " $ mMsg m) then
      -- Remove the channel name, the space and the colon and then split at spaces
      do
        let nicks = B.words $ B.drop (B.length chan + 2) $ snd $ B.breakSubstring chan $ mMsg m
        let newNicks = S.fromList $ map (\n -> if (elem . B.head) n "@+" then B.drop 1 n else n) nicks
        let newNickList = map (\(c,ns) -> if c == chan then (c, newNicks) else (c,ns)) nickList
        writeIORef ionicklist newNickList
        putStrLn $ show newNickList
        return ()
      else
      return ()
    where chan = if isJust (mChan m) then fromJust (mChan m) else (mMsg m)
          nick = if isJust (mNick m) then fromJust (mNick m) else B.pack ""
main :: IO (Either IOError MIrc)
main =
    do
      messages <- newIORef ([])
      nickList <- newIORef (map (\c -> (B.pack c,S.empty)) channelNames )
      let fileName = logName
      fhandle <- if logging then openFile fileName AppendMode else return stderr
      handle <- newIORef $ fhandle
      let events    = [(Privmsg (onPrivMsg messages nickList)), (Join (onJoinMsg messages nickList)), (Invite (onInviteMsg nickList)), (Part (onPartMsg nickList)), (RawMsg (onRawMsg handle nickList))]
      let config    = (mkDefaultConfig "" "") {
          cAddr                = "chat.freenode.net",
          cPort                = 6667,
          cSecure              = False,
          cNick                = botNick,
          cPass                = Nothing, -- Optional server password
          cUsername            = botNick,
          cRealname            = "Boten Anna",
          cChannels            = channelNames,
          cEvents              = events,
          cPingTimeoutInterval = 350 * 10^(6::Int),
          cCTCPVersion         = "Boten-Anna " ++ version
--          cCTCPTime            = fmap (formatTime defaultTimeLocale "%c") getZonedTime
       }
      info <- connect config False debugMsg
      server <- let (Right l) = info in return l
      return info

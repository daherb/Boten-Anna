{- TODO:
   - completely remove robust parsing
   - put "me" into the grammar
-}
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
import qualified Data.Text as T

-- Global config constants
version = "0.3"
botNick = "annaisnotabot"
channelNames = ["#botenannatest","#botenannatest1"]
debugMsg = False
logging = False
logName = "boten-anna.log"
pgf = readPGF "Anna.pgf"

-- Data type for the nick list
type NickList = ([(B.ByteString,S.Set B.ByteString)])

data MessageType = Msg | Png deriving Eq;

-- Data type for the message list
data Message = Envelop {
  typ :: MessageType,
  channel :: String,
  from :: String,
  to :: String,
  message :: String };
  
-- Normalize strings
normalize :: String -> String
-- Replace punctuation by spaces and make everything lower case
normalize = map (\c -> if elem c ".,!?" then ' ' else toLower c)

opUser :: String -> String -> EventFunc
opUser post response s m =
  let
      ws = words post
    in
      if length ws == 1 then
        do 
          if head ws == "me" then
            sendCmd s (MMode chan (B.pack "+o") (mNick m))
          else
            sendCmd s (MMode chan (B.pack "+o") (Just $ B.pack $ head ws))          
          sendResponse response s m
      else
        sendResponse "You are a little bit verbose, aren't you?" s m
  where chan = if isJust (mChan m) then fromJust (mChan m) else B.pack ""
        nick = if isJust (mNick m) then fromJust (mNick m) else B.pack ""
deopUser :: String -> String -> EventFunc
deopUser post response s m =
    let
      ws = words post
    in
      if length ws == 1 then
        do 
          if head ws == "me" then
            sendCmd s (MMode chan (B.pack "-o") (mNick m))
          else
            sendCmd s (MMode chan (B.pack "-o") (Just $ B.pack $ head ws))
          sendResponse response s m
      else
        sendResponse "You are a little bit verbose, aren't you?" s m
  where chan = if isJust (mChan m) then fromJust (mChan m) else B.pack ""
        nick = if isJust (mNick m) then fromJust (mNick m) else B.pack ""

pingUser :: B.ByteString -> String -> String -> IORef [Message] -> EventFunc
pingUser from post response iomessages s m = 
  let
    to = head $ words post
    rcpt = if to == "me" then B.unpack nick else to
  in
    do
      messages <- readIORef iomessages
      let newMessages = (Envelop { typ = Png, channel = B.unpack chan, from = B.unpack nick, to = rcpt, message = ""}):messages
      writeIORef iomessages newMessages
      let newResponse = T.unpack $ T.replace (T.pack "#TO#") (T.pack rcpt) $ T.replace (T.pack "#FROM#") (T.pack $ B.unpack nick) $ T.pack response
      sendResponse newResponse s m
  where chan = if isJust (mChan m) then fromJust (mChan m) else B.pack ""
        nick = if isJust (mNick m) then fromJust (mNick m) else B.pack ""
  
tellUser :: B.ByteString -> String -> String -> IORef [Message] -> EventFunc
tellUser from post response iomessages s m =
  let
    to = head $ words post
    rcpt = if to == "me" then B.unpack nick else to
    message = unwords $ tail $ words post
  in
    do
      messages <- readIORef iomessages
      let newMessages = (Envelop { typ = Msg, channel = B.unpack chan, from = B.unpack nick, to = rcpt, message = message}):messages
      writeIORef iomessages newMessages
      let newResponse = T.unpack $ T.replace (T.pack "#TO#") (T.pack rcpt) $ T.replace (T.pack "#FROM#") (T.pack $ B.unpack nick) $ T.pack response
      sendResponse newResponse s m
  where chan = if isJust (mChan m) then fromJust (mChan m) else B.pack ""
        nick = if isJust (mNick m) then fromJust (mNick m) else B.pack ""

helpUser :: String -> String -> EventFunc
helpUser pre response s m =
  if (isPrefixOf (" anna:" ) pre) then
    let nResponse = T.unpack $ T.replace (T.pack "#FROM#") (T.pack (B.unpack nick)) (T.pack response)
    in sendResponse nResponse s m
  else
    return ()
  where chan = if isJust (mChan m) then fromJust (mChan m) else B.pack ""
        nick = if isJust (mNick m) then fromJust (mNick m) else B.pack ""

impertinent :: String -> EventFunc
impertinent response s m =
  do
    sendCmd s (MMode chan (B.pack "-o") (mNick m))
    sendResponse response s m
  where chan = if isJust (mChan m) then fromJust (mChan m) else B.pack ""
        nick = if isJust (mNick m) then fromJust (mNick m) else B.pack ""

sendResponse :: String -> EventFunc
sendResponse response s m =
  do
    sendMsg s chan (B.pack response)
  where chan = if isJust (mChan m) then fromJust (mChan m) else B.pack ""
        nick = if isJust (mNick m) then fromJust (mNick m) else B.pack ""

-- Send a reply to a message
doResponse :: MIrc -> IrcMessage -> String -> [Expr] -> String -> IORef [Message] -> IO ()
doResponse s m pre [] post iomessages =
  helpUser pre ("What do you want to accomplish by saying: \"" ++ ( unwords $ tail $ words pre ) ++ "\"?") s m
doResponse s m pre parsed post iomessages =
    do
      grammar <- pgf
      -- linearize the parsed query inro a response
      let response = linearize grammar (mkCId "AnnaEngR") $ head parsed
      let action = linearize grammar (mkCId "AnnaAct") $ head parsed
      case action of 
        "OP" -> opUser post response s m ;
        "DEOP" -> deopUser post response s m ;
        "PING" -> pingUser nick post response iomessages s m ;
        "IMPOLITE PING" -> pingUser nick post response iomessages s m ;
        "TELL" -> tellUser nick post response iomessages s m ;
        "IMPOLITE TELL" -> tellUser nick post response iomessages s m ;
        "HELP" -> helpUser pre response s m ;
        "WHO_I_AM" -> helpUser pre response s m ;
        "IMPERTINENT OP" -> impertinent response s m ;
        "IMPERTINENT DEOP" -> impertinent response s m ;
        "USELESS" -> sendResponse response s m ;
        _ -> helpUser pre ("What do you want to accomplish by saying: \"" ++ ( unwords $ tail $ words pre ) ++ "\"?") s m
  where chan = if isJust (mChan m) then fromJust (mChan m) else B.pack ""
        nick = if isJust (mNick m) then fromJust (mNick m) else B.pack ""

-- Forwards messages to users on user actions
printMessages :: [Message] -> MIrc -> B.ByteString -> B.ByteString -> IO [Message]
-- No messages left, do nothing
printMessages [] _ _ _ = do return []
-- Go through all messages, see if you can find the nick as a recipient
printMessages ((msg@(Envelop {typ = tp, channel = c, from = f,to = t, message = m})):ms) s nick chan =
  -- If so forward message to him and inform the sender about the sucessful delivery
  if t == (normalize $ B.unpack nick) && c == B.unpack chan then
      if tp == Msg then
        do
          grammar <- pgf
          let msgToRcpt = T.unpack $ T.replace (T.pack "#MESSAGE#") (T.pack m) $ T.replace (T.pack "#TO#") (T.pack t) $ T.replace (T.pack "#FROM#") (T.pack f) $ T.pack $ linearize grammar (mkCId "AnnaEngR") (EFun (mkCId "tellToRcptP"))
          let msgToSnd = T.unpack $ T.replace (T.pack "#TO#") (T.pack t) $ T.replace (T.pack "#FROM#") (T.pack f) $ T.pack $ linearize grammar (mkCId "AnnaEngR") (EFun (mkCId "tellToSndP"))
          sendMsg s chan $ B.pack msgToRcpt -- ((B.unpack nick) ++ ": " ++ f ++ " wants me to tell you " ++ m)
          sendMsg s chan $ B.pack msgToSnd -- (f ++ ": Transmitted your message to " ++ (B.unpack nick))
          rest <- printMessages ms s nick chan
          return rest
      else
        do
          grammar <- pgf
          let pingToRcpt = T.unpack $ T.replace (T.pack "#TO#") (T.pack t) $ T.replace (T.pack "#FROM#") (T.pack f) $ T.pack $ linearize grammar (mkCId "AnnaEngR") (EFun (mkCId "pingToSndP"))
          let pingToSnd = T.unpack $ T.replace (T.pack "#TO#") (T.pack t) $ T.replace (T.pack "#FROM#") (T.pack f) $ T.pack $ linearize grammar (mkCId "AnnaEngR") (EFun (mkCId "tellToSndP"))
          sendMsg s chan $ B.pack pingToRcpt
          sendMsg s chan $ B.pack pingToSnd
          rest <- printMessages ms s nick chan
          return rest
   -- Otherwise continue looking
   else
     do
       rest <- printMessages ms s nick chan
       return (msg:rest)
       
-- Callback on general chat messages
onPrivMsg :: IORef ([Message]) -> IORef NickList -> EventFunc
onPrivMsg iomessages ionicks s m =
  do
    -- Get constants and parameters
    mpgf <- pgf
    messages <- readIORef iomessages
    -- Get message text and replace the bot name by the name used in grammar
    let text = T.unpack $ T.replace (T.pack botNick) (T.pack "anna") (T.pack $ B.unpack $ mMsg m)
    -- Forward messages and save the remaining ones again
    remaining <- printMessages messages s nick chan
    writeIORef iomessages remaining
    -- Try to parse
    case parseWithPGF (normalize text) mpgf (mkCId "AnnaEngQ") [mkType [] (mkCId "Placeholder") []] of
      -- No success
      Left res -> do
        -- Try to generate a response with the whole unparsed message in the pre parameter
        doResponse s m text [] "" iomessages
      -- Parse successful
      Right (pre,parsed,post) -> do
        -- Try to generate a response with the parse tree and a possible pre and post context
        doResponse s m pre parsed post iomessages
  where chan = if isJust (mChan m) then fromJust (mChan m) else B.pack ""
        nick = if isJust (mNick m) then fromJust (mNick m) else B.pack ""

-- Callback when a user joins
onJoinMsg :: IORef ([Message]) -> IORef NickList -> EventFunc
onJoinMsg iomessages ionicklist s m =
  do
    -- Get constants and parameters
    messages <- readIORef iomessages
    nickList <- readIORef ionicklist
    -- Check if it our own JOIN
    if (nick == B.pack botNick) then
      -- Yes -> Get the nicks in the channel
      do
        sendRaw s (B.pack "NAMES channel")
    else
      -- No -> try to give operator rights to user
      sendCmd s (MMode chan (B.pack "+o") (Just nick))
    -- Forward messages and save the remaining ones again
    remaining <- printMessages messages s nick chan
    writeIORef iomessages remaining
    -- Update nick list
    let newNickList = map (\(c,ns) -> if c == chan then (c, S.insert nick ns) else (c,ns)) nickList
    writeIORef ionicklist newNickList
    where chan = if isJust (mChan m) then fromJust (mChan m) else (mMsg m)
          nick = if isJust (mNick m) then fromJust (mNick m) else B.pack ""

-- Callback when the bot is invites
onInviteMsg :: IORef NickList -> EventFunc
onInviteMsg ionicklist s m =
    do
      -- Join the channel we are invited to  
      sendCmd s (MJoin (mMsg m) Nothing)
      -- Add channel to nick list
      nickList <- readIORef ionicklist
      let newNickList = if not (isNothing $ find (\(c,_) -> c == chan) nickList) then ((chan, S.empty):nickList) else nickList
      writeIORef ionicklist newNickList
    where chan = if isJust (mChan m) then fromJust (mChan m) else (mMsg m)
          nick = if isJust (mNick m) then fromJust (mNick m) else B.pack ""

-- Callback when a user leaves channel
onPartMsg :: IORef NickList -> EventFunc 
onPartMsg ionicklist s m =
  do
    -- Update nick list
    nickList <- readIORef ionicklist
    let newNickList = map (\(c,ns) -> if c == chan then (c, ns S.\\ (S.singleton nick)) else (c,ns)) nickList
    writeIORef ionicklist newNickList
    where chan = if isJust (mChan m) then fromJust (mChan m) else (mMsg m)
          nick = if isJust (mNick m) then fromJust (mNick m) else B.pack ""

-- Callback on any raw IRC message
onRawMsg :: IORef Handle -> IORef NickList -> EventFunc
onRawMsg iohandle ionicklist s m =
  do
    -- Get constants and parameters
    handle <- readIORef iohandle
    nickList <- readIORef ionicklist
    -- If enabled write the message to the log
    when logging $ hPutStrLn handle $ show m
    -- If message is of code 353 see RFC2812 RPL_NAMREPLY
    if (mCode m == B.pack "353") then
      do
        -- Get the nicks list, skip colon and split on spaces
        let nicks = B.words $ B.drop 1 $ head $ tail $ fromJust $ mOther m
        -- Remove modifiers like @ for Op
        let newNicks = S.fromList $ map (\n -> if (elem . B.head) n "@+" then B.drop 1 n else n) nicks
        -- Update nick list
        let newNickList = map (\(c,ns) -> if c == chan then (c, newNicks) else (c,ns)) nickList
        writeIORef ionicklist newNickList
        return ()
      -- Ignore other messages
      else
        return ()
    where chan = head $ fromJust (mOther m)
          nick = if isJust (mNick m) then fromJust (mNick m) else B.pack ""

-- The main function
main :: IO (Either IOError MIrc)
main =
    do
      -- Persistant memory to forward messages
      messages <- newIORef ([])
      -- Same for a list of nicks in a channel
      nickList <- newIORef (map (\c -> (B.pack c,S.empty)) channelNames )
      -- File handle for a possible log
      let fileName = logName
      fhandle <- if logging then openFile fileName AppendMode else return stderr
      handle <- newIORef $ fhandle
      -- Register event handlers
      let events    = [(Privmsg (onPrivMsg messages nickList)), (Join (onJoinMsg messages nickList)), (Invite (onInviteMsg nickList)), (Part (onPartMsg nickList)), (RawMsg (onRawMsg handle nickList))]
      -- General config
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
      -- Connect to server
      info <- connect config False debugMsg
      server <- let (Right l) = info in return l
      return info

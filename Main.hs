{- TODO:
   - completely remove robust parsing
   - put "me" into the grammar
-}
import Network.SimpleIRC
--import FakeIO -- can replace the irc interface, e.g. for testing
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

-- Replace a set of substrings
replaceInStr :: [(String,String)] -> String -> (String,[(String,String)])
replaceInStr [] src = (src,[])
replaceInStr ((pattern,rplc):rest) src =
  let (replaced,context) = replaceInStr rest src
      newContext = if isInfixOf pattern replaced then ((pattern,rplc):context) else context
      newReplaced = T.unpack $ T.replace (T.pack pattern) (T.pack $ rplc) $ T.pack $ replaced
  in
    (newReplaced,newContext)

-- Creates a EFun Expr from a string
mkFun :: String -> Expr
mkFun = EFun . mkCId
-- Extracts a receipient from parse, context and/or nick
getRcpt :: [Expr] -> [(String,String)] -> B.ByteString -> String
getRcpt parsed context nick =
  if findInAbsTrees "me" parsed
  then B.unpack nick
       -- Find first meta
  else fst $ head $ filter (\(c1,c2) -> head c2 == '?' ) context

-- (de)ops a user
changeMode :: String -> [Expr] -> String -> [(String,String)] -> String -> EventFunc
changeMode mode parsed post context response s m =
  let
      ws = words post
      rcpt = getRcpt parsed context nick
    in
      if length ws == 0 then
        do
          sendCmd s (MMode chan (B.pack mode) (Just $ B.pack $ rcpt))
          -- "you're welcome"
          sendResponse response s m
      else
        do
          grammar <- pgf
	  -- "You are a little bit verbose, aren't you?"
	  let response = linearize grammar (mkCId "AnnaEngR") (mkFun "verboseP")
          sendResponse response s m
  where chan = if isJust (mChan m) then fromJust (mChan m) else B.pack ""
        nick = if isJust (mNick m) then fromJust (mNick m) else B.pack ""

-- ops a user
opUser :: [Expr] -> String -> [(String,String)] -> String -> EventFunc
opUser parsed post context response s m =
  changeMode "+o" parsed post context response s m

-- deops a user
deopUser :: [Expr] -> String -> [(String,String)] -> String -> EventFunc
deopUser parsed post context response s m =
  changeMode "-o" parsed post context response s m

-- pings a user
pingUser :: B.ByteString -> [Expr] -> String -> [(String,String)] -> String -> IORef [Message] -> EventFunc
pingUser from parsed post context response iomessages s m = 
  let
    ws = words post
    rcpt = getRcpt parsed context nick
  in
    if length ws == 0 then
      do
        putStrLn $ "RCPT: " ++ rcpt
        messages <- readIORef iomessages
        let newMessages = (Envelop { typ = Png, channel = B.unpack chan, from = B.unpack nick, to = rcpt, message = ""}):messages
        writeIORef iomessages newMessages
        -- "#FROM#: i will ping #TO# for you"
        let newResponse = fst $ replaceInStr [("#TO#",rcpt),("#FROM#", B.unpack nick)] response
        sendResponse newResponse s m
    else
        do
          grammar <- pgf
	  -- "You are a little bit verbose, aren't you?"
	  let response = linearize grammar (mkCId "AnnaEngR") (mkFun "verboseP")
          sendResponse response s m
  where chan = if isJust (mChan m) then fromJust (mChan m) else B.pack ""
        nick = if isJust (mNick m) then fromJust (mNick m) else B.pack ""

-- forwards a message to a user
tellUser :: B.ByteString -> [Expr] -> String -> [(String,String)] -> String -> IORef [Message] -> EventFunc
tellUser from parsed post context response iomessages s m =
  let
    ws = words post
    rcpt = getRcpt parsed context nick
    -- Only skip name if it is not me
    message = post
  in
    do
      messages <- readIORef iomessages
      let newMessages = (Envelop { typ = Msg, channel = B.unpack chan, from = B.unpack nick, to = rcpt, message = message}):messages
      writeIORef iomessages newMessages
      -- "#FROM#: i will tell #TO#"
      let newResponse = fst $ replaceInStr [("#TO#", rcpt), ("#FROM#", B.unpack nick)] response
      sendResponse newResponse s m
  where chan = if isJust (mChan m) then fromJust (mChan m) else B.pack ""
        nick = if isJust (mNick m) then fromJust (mNick m) else B.pack ""

-- gives a help message
helpUser :: String -> String -> EventFunc
helpUser pre response s m =
  if (isPrefixOf (" anna:" ) pre) then
    --"#FROM#: i listen for possible commands in all conversations in this channel, so you don't have to talk to me directly. I can ''tell'' <someone> <something> or i can ''ping'' <someone> from you. if i am a channel operator i can ''op'' or ''deop'' <someone> if you ask politely. and sometime i am just stupid and annoying."
    let nResponse = fst $ replaceInStr [("#FROM#", B.unpack nick)] response
    in sendResponse nResponse s m
  else
    return ()
  where chan = if isJust (mChan m) then fromJust (mChan m) else B.pack ""
        nick = if isJust (mNick m) then fromJust (mNick m) else B.pack ""

-- reponds to impertinence 
impertinent :: String -> EventFunc
impertinent response s m =
  do
    sendCmd s (MMode chan (B.pack "-o") (mNick m))
    -- "how dare you?"
    sendResponse response s m
  where chan = if isJust (mChan m) then fromJust (mChan m) else B.pack ""
        nick = if isJust (mNick m) then fromJust (mNick m) else B.pack ""

-- send a general response
sendResponse :: String -> EventFunc
sendResponse response s m =
  do
    sendMsg s chan (B.pack response)
  where chan = if isJust (mChan m) then fromJust (mChan m) else B.pack ""
        nick = if isJust (mNick m) then fromJust (mNick m) else B.pack ""

-- creates a reply to a message
doResponse :: MIrc -> IrcMessage -> String -> [Expr] -> String -> [(String,String)] -> IORef [Message] -> IO ()
doResponse s m pre [] post context iomessages =
  do
    grammar <- pgf
    -- "What do you want to accomplish by saying: \"#MESSAGE#\""
    let response = linearize grammar (mkCId "AnnaEngR") (mkFun "accomplishP")
    let message = unwords $ tail $ words pre
    let newResponse = fst $ replaceInStr [("#MESSAGE#",message)] response
    trace "NO PARSE" helpUser pre newResponse s m
    
doResponse s m pre parsed post context iomessages =
    do
      grammar <- pgf
      -- linearize the parsed query inro a response
      let response = linearize grammar (mkCId "AnnaEngR") $ head parsed
      let action = linearize grammar (mkCId "AnnaAct") $ head parsed
      putStrLn $ "ACTION: " ++ action
      case action of 
        "OP" -> opUser parsed post context response s m ;
        "DEOP" -> deopUser parsed post context response s m ;
        "PING" -> pingUser nick parsed post context response iomessages s m ;
        "IMPOLITE PING" -> pingUser nick parsed post context response iomessages s m ;
        "TELL" -> tellUser nick parsed post context response iomessages s m ;
        "IMPOLITE TELL" -> tellUser nick parsed post context response iomessages s m ;
        "HELP" -> helpUser pre response s m ;
        "WHO_I_AM" -> helpUser pre response s m ;
        "IMPERTINENT OP" -> impertinent response s m ;
        "IMPERTINENT DEOP" -> impertinent response s m ;
        "IMPOLITE OP" -> sendResponse response s m ;
        "IMPOLITE DEOP" -> sendResponse response s m ;
        "USELESS" -> sendResponse response s m ;
        "ABOUT_ME" -> sendResponse response s m ;
        "NO_BOT" -> sendResponse response s m ;
        _ -> trace "LAST CASE " $ doResponse s m pre [] post [] iomessages 
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
          -- "#TO#: #FROM# wants me to tell you #MESSAGE#"
          let msgToRcpt = fst $ replaceInStr [("#MESSAGE#", m), ("#TO#", t), ("#FROM#", f)] $ linearize grammar (mkCId "AnnaEngR") $ mkFun "tellToRcptP"
          -- "#FROM#: transmitted your message to #TO#"
          let msgToSnd = fst $ replaceInStr [("#TO#", t), ("#FROM#", f)] $ linearize grammar (mkCId "AnnaEngR") $ mkFun "tellToSndP"
          sendMsg s chan $ B.pack msgToRcpt
          sendMsg s chan $ B.pack msgToSnd
          rest <- printMessages ms s nick chan
          return rest
      else
        do
          grammar <- pgf
          -- "#TO#: #FROM# wants me to ping you"
          let pingToRcpt = fst $ replaceInStr [("#TO#", t), ("#FROM#", f)] $ linearize grammar (mkCId "AnnaEngR") $ mkFun "pingToRcptP"
          -- "#FROM#: pinged #TO# for you"
          let pingToSnd = fst $ replaceInStr [("#TO#", t), ("#FROM#", f)] $ linearize grammar (mkCId "AnnaEngR") $ mkFun "pingToSndP"
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
    nickList <- readIORef ionicks
    -- Get message text and replace the bot name by the name used in grammar
    let replaceList = [(botNick,"anna")] -- ++ (S.toList $ S.map (\n -> (B.unpack n,"#NICK#")) $ snd $ head $ filter (\(c,_) -> c == chan) nickList)
    let (text,context) = replaceInStr replaceList $ normalize $ B.unpack $ mMsg m
    -- Forward messages and save the remaining ones again
    remaining <- printMessages messages s nick chan
    writeIORef iomessages remaining
    -- Try to parse
    let (pre,parsed,post,ncontext) = parseOpenWithPGF text mpgf (mkCId "AnnaEngQ") [(fromJust $ readType "Placeholder")]
    putStrLn $ "pre: " ++ pre ++ " parsed: " ++ show parsed ++ " post: " ++ post ++ " context: " ++ show ncontext
    doResponse s m pre parsed post (context ++ ncontext) iomessages
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

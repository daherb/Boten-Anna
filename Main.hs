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
logging = True
logName = "boten-anna.log"
pgf = readPGF "Anna.pgf"

-- Data type for the nick list
type NickList = ([(B.ByteString,S.Set B.ByteString)])

-- Data type for the message list
data Message = Envelop {
  channel :: String,
  from :: String,
  to :: String,
  message :: String };
  
-- Normalize strings
normalize :: String -> String
-- Replace punctuation by spaces and make everything lower case
normalize = map (\c -> if elem c ".,!?" then ' ' else toLower c)

opUser :: String -> String -> IO ()
opUser post response = return ();

deopUser :: String -> String -> IO ()
deopUser post response = return ();

pingUser :: B.ByteString -> String -> String -> IO ()
pingUser from post response = return ();

tellUser :: B.ByteString -> String -> String -> IO ()
tellUser from post response = return ()

helpUser :: B.ByteString -> String -> String -> String -> IO ()
helpUser from pre post response = return ();

sendResponse :: String -> IO ()
sendResponse response = return ();

-- Send a reply to a message
doResponse :: MIrc -> IrcMessage -> String -> [Expr] -> String -> IORef [Message] -> IO ()
doResponse s m pre parsed post iomessages =
    do
      grammar <- pgf
      -- linearize the parsed query inro a response
      let response = linearize grammar (mkCId "AnnaEngR") $ head parsed
      let action = linearize grammar (mkCId "AnnaAct") $ head parsed
      case action of 
        "OP" -> opUser post response ;
        "DEOP" -> deopUser post response ;
        "PING" -> pingUser nick post response ;
        "IMPOLITE PING" -> pingUser nick post response ;
        "TELL" -> tellUser nick post response ;
        "IMPOLITE TELL" -> tellUser nick post response ;
        "HELP" -> helpUser nick pre post response ;
        _ -> sendResponse response
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

-- Forwards messages to users on user actions
printMessages :: [Message] -> MIrc -> B.ByteString -> B.ByteString -> IO [Message]
-- No messages left, do nothing
printMessages [] _ _ _ = do return []
-- Go through all messages, see if you can find the nick as a recipient
printMessages ((msg@(Envelop {channel = c, from = f,to = t, message = m})):ms) s nick chan =
  -- If so forward message to him and inform the sender about the sucessful delivery
  if t == (normalize $ B.unpack nick) && c == B.unpack chan then
    do
      sendMsg s chan $ B.pack ((B.unpack nick) ++ ": " ++ f ++ " wants me to tell you " ++ m)
      sendMsg s chan $ B.pack (f ++ ": Transmitted your message to " ++ (B.unpack nick))
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
    putStrLn ("###" ++ text)
    -- Try to parse
    case parseWithPGF (normalize text) mpgf (mkCId "EngQ") [mkType [] (mkCId "Placeholder") []] of
      -- No success
      Left res -> do
        putStrLn $ show res
        -- Try to generate a response with the whole unparsed message in the pre parameter
        doResponse s m text [] "" iomessages
      -- Parse successful
      Right (pre,parsed,post) -> do
        putStrLn $ "Pre: " ++ pre ++ " Parse trees: " ++ (show parsed) ++ " Post: " ++ post ++ " EOL"
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
    putStrLn $ show newNickList
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
    putStrLn $ show newNickList
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
    if ((B.isInfixOf . B.pack) " 353 " $ mMsg m) then
      do
        -- Remove the channel name, the space and the colon and then split at spaces
        let nicks = B.words $ B.drop (B.length chan + 2) $ snd $ B.breakSubstring chan $ mMsg m
        -- Remove modifiers like @ for Op
        let newNicks = S.fromList $ map (\n -> if (elem . B.head) n "@+" then B.drop 1 n else n) nicks
        -- Update nick list
        let newNickList = map (\(c,ns) -> if c == chan then (c, newNicks) else (c,ns)) nickList
        writeIORef ionicklist newNickList
        putStrLn $ show newNickList
        return ()
      -- Ignore other messages
      else
        return ()
    where chan = if isJust (mChan m) then fromJust (mChan m) else (mMsg m)
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

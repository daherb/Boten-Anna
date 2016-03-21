module FakeIO (MIrc,
               EventFunc,
               Command(..),
               IrcMessage(..),
               IrcConfig(..),
               IrcEvent(..),
               connect,
               mkDefaultConfig,
               sendCmd,
               sendRaw,
               sendMsg
              ) where
import qualified Data.ByteString.Char8 as B

type MIrc = ()
type EventFunc = MIrc -> IrcMessage -> IO ()

data Command =
    MPrivmsg B.ByteString B.ByteString                      -- ^ PRIVMSG #chan :msg
  | MJoin    B.ByteString (Maybe B.ByteString)              -- ^ JOIN #chan key
  | MPart    B.ByteString B.ByteString                      -- ^ PART #chan :msg
  | MMode    B.ByteString B.ByteString (Maybe B.ByteString) -- ^ MODE #chan +o user
  | MTopic   B.ByteString (Maybe B.ByteString)              -- ^ TOPIC #chan :topic
  | MInvite  B.ByteString B.ByteString                      -- ^ INVITE user #chan
  | MKick    B.ByteString B.ByteString B.ByteString         -- ^ KICK #chan user :msg
  | MQuit    B.ByteString                                   -- ^ QUIT :msg
  | MNick    B.ByteString                                   -- ^ NICK newnick
  | MNotice  B.ByteString B.ByteString                      -- ^ NOTICE usr/#chan :msg
  | MAction  B.ByteString B.ByteString                      -- ^ PRIVMSG usr/#chan :ACTION msg
  deriving (Eq, Read, Show)

data IrcConfig = IrcConfig
  { cAddr     :: String   -- ^ Server address to connect to
  , cPort     :: Int      -- ^ Server port to connect to
  , cSecure   :: Bool     -- ^ Use secure transport
  , cNick     :: String   -- ^ Nickname
  , cPass     :: Maybe String -- ^ Optional server password
  , cUsername :: String   -- ^ Username
  , cRealname :: String   -- ^ Realname
  , cChannels :: [String]   -- ^ List of channels to join on connect
  , cEvents   :: [IrcEvent] -- ^ Events to bind
  , cCTCPVersion :: String  -- ^ What to send on CTCP VERSION
  , cCTCPTime    :: IO String  -- ^ What to send on CTCP TIME
  , cPingTimeoutInterval :: Int -- The time between server messages that causes ping timeout
  }

data IrcEvent =
    Privmsg EventFunc -- ^ PRIVMSG
  | Numeric EventFunc -- ^ Numeric, 001, 002, 372 etc.
  | Ping EventFunc    -- ^ PING
  | Join EventFunc    -- ^ JOIN
  | Part EventFunc    -- ^ PART
  | Mode EventFunc    -- ^ MODE
  | Topic EventFunc   -- ^ TOPIC
  | Invite EventFunc  -- ^ INVITE
  | Kick EventFunc    -- ^ KICK
  | Quit EventFunc    -- ^ QUIT
  | Nick EventFunc    -- ^ NICK
  | Notice EventFunc  -- ^ NOTICE
  | RawMsg EventFunc  -- ^ This event gets called on every message received
  | Disconnect (MIrc -> IO ()) -- ^ This event gets called whenever the
    --   connection with the server is dropped

data IrcMessage = IrcMessage
  { mNick   :: Maybe B.ByteString
  , mUser   :: Maybe B.ByteString
  , mHost   :: Maybe B.ByteString
  , mServer :: Maybe B.ByteString
  , mCode   :: B.ByteString
  , mMsg    :: B.ByteString
  , mChan   :: Maybe B.ByteString
  , mOrigin :: Maybe B.ByteString   -- ^ Origin of the message, this is mNick if a message was sent directly to the bot, otherwise if it got sent to the channel it's mChan.
  , mOther  :: Maybe [B.ByteString]
  , mRaw    :: B.ByteString
  } deriving (Show)

connect :: IrcConfig -> Bool -> Bool -> IO (Either IOError MIrc)
connect config _ _ =
  do
    let callback = (\(Privmsg e) -> e) $ head $ filter (\c -> case c of (Privmsg e) -> True ; _ -> False) $ cEvents config
    loop callback
    return $ Right ()
loop :: EventFunc -> IO ()
loop callback =
  do
    putStr "> "
    line <- getLine
    callback () $ IrcMessage { mNick   = Just $ B.pack "testnick"
                  , mUser   = Nothing
                  , mHost   = Nothing
                  , mServer = Nothing
                  , mCode   = B.pack "PRIVMSG"
                  , mMsg    = B.pack line
                  , mChan   = Just $ B.pack "testchan"
                  , mOrigin = Nothing
                  , mOther  = Nothing
                  , mRaw    = B.pack "" }
    loop callback
  
mkDefaultConfig :: String -> String -> IrcConfig
mkDefaultConfig _ _ = IrcConfig "" 0 False "" Nothing "" "" [] [] "" (return "") 0

sendCmd :: MIrc -> Command -> IO ()
sendCmd _ c = do putStrLn $ "<COMMAND: " ++ show c

sendRaw :: MIrc -> B.ByteString -> IO ()
sendRaw _ _ = do return ()

sendMsg :: MIrc -> B.ByteString -> B.ByteString -> IO ()
sendMsg _ channel msg = putStrLn $ "<MSG: " ++ (B.unpack channel) ++ ": " ++ (B.unpack msg)

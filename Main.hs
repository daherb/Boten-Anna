import Network.SimpleIRC
import Data.Maybe
import qualified Data.ByteString.Char8 as B

botnick = "annaärinteenbot"
channelname = "#esslli2015"
              
onPrivMsg :: EventFunc
onPrivMsg s m
    -- Bound by laws of robotics https://en.wikipedia.org/wiki/Robotic_laws
    | fst (B.breakSubstring (B.pack "kill") msg) /= msg || fst (B.breakSubstring (B.pack "hurt") msg) /= msg || fst (B.breakSubstring (B.pack "order") msg) /= msg  = do
      sendMsg s chan (B.pack "I am bound by Asimov's laws")
    -- other rules
    |  B.isPrefixOf (B.pack (botnick ++ ":")) msg = do
      sendMsg s chan (B.append (B.pack "What do you want to accomplish by saying: \"") (B.append (B.drop ((length botnick) + 2) msg) (B.pack "\"?")))
    | fst (B.breakSubstring (B.pack " bot") msg) /= msg = do
      sendMsg s chan (B.pack "I am not a bot")
    | (fst (B.breakSubstring (B.pack " anna") msg) /= msg) && ((fromJust $ mNick m) /= (B.pack botnick)) = do
      sendMsg s chan (B.pack "Are you talking about me?")
    | True =
        do
          putStr "Privmsg: "
          putStrLn $ show m
    where chan = fromJust $ mChan m
          msg = mMsg m
                
onJoinMsg :: EventFunc
onJoinMsg s m =
    do
      putStr "Joinmsg: "
      putStrLn $ show m
      sendCmd s (MMode chan (B.pack "+o") (mNick m))
    where chan = B.pack channelname
                 
events = [(Privmsg onPrivMsg), (Join onJoinMsg)]
           
freenode = (mkDefaultConfig "irc.freenode.net" botnick)
           {
             cChannels = [channelname], -- Channels to join on connect
             cEvents   = events -- Events to bind
           }                                                                                                                               

main =
    do
      connect freenode False True

import Network.SimpleIRC
import Data.Maybe
import qualified Data.ByteString.Char8 as B

botname = "anaisnotabot"
channelname = "#esslli2015"
              
onPrivMsg :: EventFunc
onPrivMsg s m
    |  B.isPrefixOf (B.pack botname) msg = do
      sendMsg s chan (B.append (B.pack "What do you want by saying: \"") (B.append (B.drop ((length botname) + 2) msg) (B.pack "\"?")))
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
    where chan = fromJust $ mChan m
                 
events = [(Privmsg onPrivMsg), (Join onJoinMsg)]
           
freenode = (mkDefaultConfig "irc.freenode.net" "annaisnotabot")
           {
             cChannels = ["#botenannastestchannel"], -- Channels to join on connect
             cEvents   = events -- Events to bind
           }                                                                                                                               

main =
    do
      connect freenode False True

import Network.SimpleIRC
import Data.Maybe
import qualified Data.ByteString.Char8 as B

botnick = "anaisnotabot"
channelname = "#botenannastestchannel" --"#esslli2015"
              
onPrivMsg :: EventFunc
onPrivMsg s m
    |  B.isPrefixOf (B.pack botnick) msg = do
      sendMsg s chan (B.append (B.pack "What do you want to accomplish by saying: \"") (B.append (B.drop ((length botnick) + 2) msg) (B.pack "\"?")))
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

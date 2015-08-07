import Network.SimpleIRC
import Data.Maybe
import qualified Data.ByteString.Char8 as B

onPrivMsg :: EventFunc
onPrivMsg s m =
    do
      putStr "Privmsg: "
      putStrLn $ show m
onJoinMsg :: EventFunc
onJoinMsg s m =
    do
      putStr "Joinmsg: "
      putStrLn $ show m
      sendCmd s (MMode (B.pack (head (cChannels freenode))) (B.pack "+o") (mNick m))
                 
events = [(Privmsg onPrivMsg), (Join onJoinMsg)]
           
freenode = (mkDefaultConfig "irc.freenode.net" "annaisnotabot")
           {
             cChannels = ["#botenannastestchannel"], -- Channels to join on connect
             cEvents   = events -- Events to bind
           }                                                                                                                               

main =
    do
      connect freenode False True

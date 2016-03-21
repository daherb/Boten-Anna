concrete AnnaEngR of Anna = open Prelude in {
  param
    Cmd = DeopC | HelpC | OpC | PingC | TellC ;
    Nme = Plchldr | Anna | Me | None ;
  oper
    BotT : Type = { bot : Bool } ;
    NameT : Type = { name : Nme } ;
    CommandT: Type = { command : Cmd } ;
    PoliteT : Type = { polite : Bool };
  lincat
    Bot = BotT ;
    Name = NameT ;
    Command = BotT ** NameT ** CommandT ** PoliteT ;
    Comment = BotT ** NameT ** { str : Str } ;
    Placeholder = NameT ;
    S = Str;
    Deop, Help, Op, Ping, Tell = CommandT ; 
  lin
    bot = { bot = True } ;
    anna = { name = Anna } ;
    me = { name = Me } ;
    canyou c = c ;
    name n c = n ** c ;
    please c = c ** { polite = True } ;
    -- please c = c  ;
    tell = { command = TellC } ;
    op = { command = OpC } ;
    deop = { command = DeopC } ;
    help = { command = HelpC } ;
    ping = { command = PingC } ;
    commandS c = "" ++
      case <c.command,c.name,c.polite> of {
	<OpC | DeopC,_,False> => "you have to be more polite if I should help you" ;
	<OpC | DeopC,Anna,_> => "how dare you?" ;
	<OpC | DeopC,_,True> => "you're welcome" ;
	<TellC | PingC,Anna,_> => "that's rather useless, isn't it?" ;
	<TellC,_,_> => "#FROM#: i will tell #TO#" ;
	<PingC,_,_> => "#FROM#: i will ping #TO# for you" ;
	<HelpC,_,_> => "#FROM#: i listen for possible commands in all conversations in this channel, so you don't have to talk to me directly. I can ''tell'' <someone> <something> or i can ''ping'' <someone> from you. if i am a channel operator i can ''op'' or ''deop'' <someone> if you ask politely. and sometime i am just stupid and annoying." ;
	_ => nonExist
      } ;
    commentS c =
      case c.name of {
	Anna => "are you talking about me?" ;
	_ => case c.bot of {
	  True => "i am not a bot" ;
	  False => c.str
	  }
      };
    botC b = b ** { name = None ; str = "" } ;
    nameC n = n ** { bot = False ; str = "" } ;
    whoareyouC = { bot = False ; name = None ; str = "hi, i'm anna. i can *help* you" } ; 
    tellC c p = c ** { bot = False; name = None ; polite = False } ;
    opC c p = c ** p ** { bot = False ; polite = False } ;
    deopC c p = c ** p ** { bot = False ; polite = False } ;
    helpC c = c ** { bot = False ; name = None ; polite = False } ;
    pingC c p = c ** p ** { bot = False ; polite = False } ;
    nick = { name = Plchldr } ;
    nameP n = lin Name n ;
    verboseP = "you are a little bit verbose, aren't you?" ;
    tellToRcptP = "#TO#: #FROM# wants me to tell you #MESSAGE#" ;
    tellToSndP = "#FROM#: transmitted your message to #TO#" ;
    pingToRcptP = "#TO#: #FROM# wants me to ping you" ;
    pingToSndP = "#FROM#: pinged #TO# for you" ;
    accomplishP = "What do you want to accomplish by saying: \"#MESSAGE#\"" ;
}
 
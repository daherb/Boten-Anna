concrete AnnaEngR of Anna = open Prelude in {
  param
    Cmd = DeopC | HelpC | OpC | PingC | TellC ;
  oper
    BotT : Type = { bot : Bool } ;
    NameT : Type = { name : Bool } ;
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
    anna = { name = True } ;
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
      <TellC,False,_> => "#FROM#: i will tell #TO#" ;
      <OpC | DeopC,False,True> => "you're welcome" ;
      <OpC | DeopC,_,False> => "you have to be more polite if I should help you" ;
      <OpC | DeopC,True,_> => "how dare you?" ;
      <HelpC,_,_> => "#FROM#: i listen for possible commands in all conversations in this channel, so you don't have to talk to me directly. I can ''tell'' <someone> <something> or i can ''ping'' <someone> from you. if i am a channel operator i can ''op'' or ''deop'' <someone> if you ask politely. and sometime i am just stupid and annoying." ;
      <PingC,False,_> => "#FROM#: i will ping #TO# for you" ;
      <TellC | PingC,True,_> => "that's rather useless, isn't it?" ;
      _ => nonExist
      } ;
    commentS c =
      case c.name of {
	True => "are you talking about me?" ;
	False => case c.bot of {
	  True => "i am not a bot" ;
	  False => c.str
	  }
      };
    botC b = b ** { name = False ; str = "" } ;
    nameC n = n ** { bot = False ; str = "" } ;
    whoareyouC = { bot = False; name = False; str = "hi, i'm anna. i can *help* you" } ; 
    tellC c p = c ** { bot = False; name = False ; polite = False } ;
    opC c p = c ** p ** { bot = False ; polite = False } ;
    deopC c p = c ** p ** { bot = False ; polite = False } ;
    helpC c = c ** { bot = False ; name = False ; polite = False } ;
    pingC c p = c ** p ** { bot = False ; polite = False } ;
    nick = { name = False } ;
    nameP n = lin Name n ;
}
 
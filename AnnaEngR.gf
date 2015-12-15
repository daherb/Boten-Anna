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
    Comment = BotT ** NameT ;
    Placeholder = NameT ;
    S = Str;
    Deop, Help, Op, Ping, Tell = CommandT ; 
  lin
    bot = { bot = True } ;
    anna = { name = True } ;
    canyou c = c ;
    name n c = n ** c ;
    --    please c = c ** { polite = True } ;
        please c = c  ;
    tell = { command = TellC } ;
    op = { command = OpC } ;
    deop = { command = DeopC } ;
    help = { command = HelpC } ;
    ping = { command = PingC } ;
    commandS c = "" ++
    case c.command of {
      TellC => "#FROM#: i will tell #TO#" ;
       	OpC | DeopC => case c.polite of {
       	  True => "you're welcome" ;
       	  False => "you have to be more polite if I should help you"
      } ;
      HelpC => "#FROM#: i listen for possible commands in all conversations in this channel, so you don't have to talk to me directly. I can ''tell'' <someone> <something> or i can ''ping'' <someone> from you. if i am a channel operator i can ''op'' or ''deop'' <someone> if you ask politely. and sometime i am just stupid and annoying." ;
      PingC => "#FROM#: i will ping #TO# for you"
	
      } ;
    commentS c =
      case c.name of {
	True => "are you talking about me?" ;
	False => case c.bot of {
	  True => "i am not a bot" ;
	  False => nonExist
	  }
      };
    botC b = b ** { name = False } ;
    nameC n = n ** { bot = False } ;
    tellC c p = c ** { bot = False; name = False ; polite = False } ;
    opC c p = c ** p ** { bot = False ; polite = False } ;
    deopC c p = c ** p ** { bot = False ; polite = False } ;
    helpC c = c ** { bot = False ; name = False ; polite = False } ;
    pingC c p = c ** p ** { bot = False ; polite = False } ;
    nick = { name = False } ;
    nameP n = lin Name n ;
}
 
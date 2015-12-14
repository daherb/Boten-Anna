concrete AnnaEngR of Anna = open Prelude in {
  param
    Cmd = DeopC | HelpC | OpC | PingC | TellC ;
  oper
    BotT : Type = { bot : Bool } ;
    NameT : Type = { name : Bool } ;
    CommandT: Type = { command : Str } ;
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
    tell = { command = "tell"} ;
    op = { command = "op" } ;
    deop = { command = "deop" } ;
    help = { command = "help" } ;
    ping = { command = "ping" } ;
    commandS c = "" ++
      case c.command of {
      -- 	"tell" => "#FROM#: i will tell #TO#" ;
      -- 	"op" | "deop" => case c.polite of {
      -- 	  True => "you're welcome" ;
      -- 	  False => "you have to be more polite if I should help you"
      -- 	  } ;
      -- 	"help" => "Bla Fasel" ;
	Ping => "#FROM#: i will ping #TO# for you"
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
 
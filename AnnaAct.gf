concrete AnnaAct of Anna = open Prelude in {
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
    please c = c ** { polite = True } ;
    -- please c = c  ;
    tell = { command = TellC } ;
    op = { command = OpC } ;
    deop = { command = DeopC } ;
    help = { command = HelpC } ;
    ping = { command = PingC } ;
    commandS c = "" ++
      case <c.command,c.name,c.polite> of {
      <TellC,False,_> => "TELL" ;
      <OpC,False,True> => "OP" ;
      <DeopC,False,True> => "DEOP" ;
      <OpC,_,False> => "IMPOLITE OP" ;
      <DeopC,_,False> => "IMPOLITE DEOP" ;
      <OpC,True,_> => "IMPERTINENT OP" ;
      <DeopC,True,_> => "IMPERTINENT DEOP" ;
      <HelpC,_,_> => "HELP" ;
      <PingC,False,_> => "PING" ;
      <TellC | PingC,True,_> => "USELESS" ;
      _ => nonExist
      } ;
    commentS c =
      case c.name of {
	True => "ABOUT_ME" ;
	False => case c.bot of {
	  True => "NO_BOT" ;
	  False => "WHO_I_AM"
	  }
      };
    botC b = b ** { name = False } ;
    nameC n = n ** { bot = False } ;
    whoareyouC = { name = False ; bot = False } ;
    tellC c p = c ** { bot = False; name = False ; polite = False } ;
    opC c p = c ** p ** { bot = False ; polite = False } ;
    deopC c p = c ** p ** { bot = False ; polite = False } ;
    helpC c = c ** { bot = False ; name = False ; polite = False } ;
    pingC c p = c ** p ** { bot = False ; polite = False } ;
    nick = { name = False } ;
    nameP n = lin Name n ;
    verboseP = nonExist ;
    tellToRcptP = nonExist ;
    tellToSndP = nonExist ;
    pingToRcptP = nonExist ;
    pingToSndP = nonExist ;
    accomplishP = nonExist ;
}
 
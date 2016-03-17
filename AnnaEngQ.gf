concrete AnnaEngQ of Anna = open Prelude in {
  param
    Case = Nom | Gen ;
    Number = Sg | Pl ;
  lincat
    Command, Op, Deop, Tell, Help, Ping = { pre1 : Str ; pre2 : Str ; mid : Str };
    Comment, Bot, Name = Number => Case => Str;
    S = Str;
    Placeholder = SS ;
  lin
    bot = table {
      Sg => table { Nom => "bot" ; Gen => "bots" };
      Pl => table { _ => "bots" }
      };
    anna = table {
      Sg => table { Nom => "anna" ; Gen => "annas" } ;
      Pl => \\_ => nonExist
      };
    me = table {
      Sg => table { Nom => "me" ; Gen => "my" } ;
      Pl => table { Nom => "we" ; Gen => "our" }
      } ;
    canyou c = c ** { pre1 = "can you" | "could you" } ;
    name n c = c ** { pre1 = n ! Sg ! Nom ++ c.pre1 } ;
    please c = c ** { pre2 = "please" } ;
    tell = { pre1 = "" ; pre2 = "" ; mid = "tell" ; } ;
    op = { pre1 = "" ; pre2 = "" ;  mid = "op" } ;
    deop = { pre1 = "" ; pre2 = "" ;  mid = "de-op" | "deop" } ;
    help = { pre1 = "" ; pre2 = "" ; mid = "help" } ;
    ping = { pre1 = "" ; pre2 = "" ; mid = "ping" } ;
    commandS c = c.pre1 ++ c.pre2 ++ c.mid ;
    commentS c =
      let
        n = Sg | Pl ;
	cs = Nom | Gen ;
      in
      c ! n ! cs ;
    botC c = c ;
    nameC c = c ;
    whoareyouC = \\_,_ => "who are you" ;
    tellC c p = c ** { mid = c.mid ++ p.s } ;
    opC c p = c ** { mid = c.mid ++ p.s } ;
    deopC c p = c ** { mid = c.mid ++ p.s } ;
    helpC c = c ;
    pingC c p = c ** { mid = c.mid ++ p.s } ;
    nick = ss "#NICK#" ;
    nameP n = ss (n ! Sg ! Nom );
    verboseP = nonExist ;
    tellToRcptP = nonExist ;
    tellToSndP = nonExist ;
    pingToRcptP = nonExist ;
    pingToSndP = nonExist ;
    accomplishP = nonExist ;
}
 
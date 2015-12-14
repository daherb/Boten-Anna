concrete AnnaGerQ of Anna = open Prelude in {
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
    canyou c = c ** { pre1 = "kannst du" } ;
    name n c = c ** { pre1 = n ! Sg ! Nom ++ c.pre1 } ;
    please c = c ** { pre2 = "bitte" } ;
    tell = { pre1 = "" ; pre2 = "" ; mid = "sagen" ; } ;
    op = { pre1 = "" ; pre2 = "" ;  mid = "op-en" | "open" } ;
    deop = { pre1 = "" ; pre2 = "" ;  mid = "de-op-en" | "deopen" } ;
    help = { pre1 = "" ; pre2 = "" ; mid = "helfen" } ;
    ping = { pre1 = "" ; pre2 = "" ; mid = "pingen" } ;
    commandS c = c.pre1 ++ c.pre2 ++ c.mid ;
    commentS c =
      let
        n = Sg | Pl ;
	cs = Nom | Gen ;
      in
      c ! n ! cs ;
    botC c = c ;
    nameC c = c ;
    tellC c p = c ** { mid = p.s ++ c.mid} ;
    opC c p = c ** { mid = p.s ++ c.mid} ;
    deopC c p = c ** { mid = p.s ++ c.mid} ;
    helpC c = c ;
    pingC c p = c ** { mid = p.s ++ c.mid} ;
    nick1 = ss "#NICK#" ;
    nick2 = ss "#NICK#" ;
}
 
concrete Anna of AnnaAbs = open Prelude in {
  param
    Case = Nom | Gen ;
    Number = Sg | Pl ;
  lincat
    Command, Op, Deop, Tell = { pre1 : Str ; pre2 : Str ; mid : Str };
    Comment, Bot, Name = Number => Case => Str;
    S = Str;
  lin
    bot = table {
      Sg => table { Nom => "bot" ; Gen => "bots" };
      Pl => table { _ => "bots" }
      };
    anna = table {
      Sg => table { Nom => "anna" ; Gen => "annas" } ;
      Pl => \\_ => nonExist
      };
    canyou c = c ** { pre1 = "can you" } ;
    name n c = c ** { pre1 = n ! Sg ! Nom ++ c.pre1 } ;
    please c = c ** { pre2 = "please" } ;
    tell = { pre1 = "" ; pre2 = "" ; mid = "tell" ; } ;
    op = { pre1 = "" ; pre2 = "" ;  mid = "op" } ;
    deop = { pre1 = "" ; pre2 = "" ;  mid = "de-op" | "deop" } ;
    commandS c = c.pre1 ++ c.pre2 ++ c.mid ;
    commentS c =
      let
        n = Sg | Pl ;
	cs = Nom | Gen ;
      in
      c ! n ! cs ;
    botC c = c ;
    nameC c = c ;
    tellC c = c ;
    opC c = c ;
    deopC c = c ;
}
 
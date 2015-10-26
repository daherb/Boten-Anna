concrete Anna of AnnaAbs = open Prelude in {
  param
    Case = Nom | Gen ;
    Number = Sg | Pl ;
  lincat
    Command = { pre1 : Str ; pre2 : Str ; mid : Str };
    Comment = Number => Case => Str;
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
    please c = c ** { pre2 = "please" } ; 
    tell = { pre1 = "" ; pre2 = "" ; mid = "tell" ; } ;
    op = { pre1 = "" ; pre2 = "" ;  mid = "op" } ;
    deop = { pre1 = "" ; pre2 = "" ;  mid = "de-op" } ;
    commandS c = c.pre1 ++ c.pre2 ++ c.mid ;
    commentS c =
      let
        n = Sg | Pl ;
	cs = Nom | Gen ;
      in
        c ! n ! cs ;
      
}
 
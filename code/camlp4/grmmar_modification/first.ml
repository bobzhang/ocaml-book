open Camlp4.PreCast;
open Format;
value test = Gram.Entry.mk "test";
value p = Gram.Entry.print std_formatter;

EXTEND Gram GLOBAL: test;
  test:
    ["add" LEFTA
	[SELF; "+" ; SELF | SELF; "-" ; SELF]
    | "mult" RIGHTA
	[SELF; "*" ; SELF | SELF; "/" ; SELF]
    | "simple" NONA
	[ "("; SELF; ")"  | INT ] ];
END;
p  test;

EXTEND Gram GLOBAL: test;
    test: [[ x = SELF ; "plus1plus" ; y = SELF ]];
END ;

p test;

EXTEND Gram  test: LAST
    [[x = SELF ; "plus1plus" ; y = SELF ]];
  END;
p test ;


EXTEND Gram
  test: LEVEL "mult"
  [[x = SELF ; "plus1plus" ; y = SELF ]];
END ;

p  test;


EXTEND Gram
  test: BEFORE "mult" [[x = SELF ; "plus1plus" ; y = SELF ]];
END ;

p  test;

  
  




















open Format;
open Camlp4.PreCast;

value c = Gram.Entry.mk "c";
open Gram;
value parse_c = Camlp4ext.parse_string_of_entry c;
value mkc key =
  EXTEND Gram GLOBAL: c;
    c:
    [[  KEYWORD $key$; "A"; "B" -> ()]];
  END;

value mkd key =
  EXTEND Gram GLOBAL: c;
    c:
    [[  "A" -> ()]];
  END;

mkc "B";

parse_c "B A B" ;

mkc "A";

parse_c "A A B" ;















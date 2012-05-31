open Format;

type a =
  [ A of b and float | C]
and b =
  [ B of a | D ]
;

class fold = Camlp4FoldGenerator.generated;

  




















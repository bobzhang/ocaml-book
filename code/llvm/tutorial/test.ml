(* -*- Mode:Tuareg; -*-
   *===----------------------------------------------------------------------===
   * Version: $Id: test.ml,v 0.0 2012/02/21 15:45:06 bobzhang1988 Exp $
   *===----------------------------------------------------------------------===*)




def foo (a,b)
 a * a + 2 * a * b + b * b
;

def bar(a)
 foo(a,4.0) + bar(31337)
;


extern cos(x)
;

extern sin(x)
;

def ff(x)
  sin(x) * sin(x) + cos(x) * cos(x)
;

ff(32.0)
;

cos(1.234)
;


def test(x)
  1 + 2 + x
;

def test2(x)
  (1+2+x) * (x+(1+2))
;  

4+5
;



















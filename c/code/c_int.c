
/* @(#)c_int.c
 */


#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <stdio.h>

value
Cint_of_OCAMLint (value v){
  fprintf(stderr,"cint_of_ml_int\n");
  value res = caml_alloc(1,Abstract_tag);
  fprintf(stderr,"haha");
  Field(res,0) = Long_val(v);
  fprintf(stderr,"stored");
  return res;
}


value
OCAMLint_of_Cint(value v){
  return Val_long(Field(v,0));
}

value
Cplus(value v1,value v2){
  fprintf(stderr,"cplus\n");
  value res = caml_alloc(1,Abstract_tag);
  Field(res,0) = Field(v1,0) + Field(v2,0);
  return res;
}

value
printCint(value v){
  printf("%ld", (long)(Field(v,0)));
  fflush(stdout);
  return Val_unit;
}

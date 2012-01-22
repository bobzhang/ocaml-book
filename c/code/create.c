#include <malloc/malloc.h>
#include <stdio.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>

typedef struct{
  int size;
  long * tab;
} IntTab;

IntTab*
alloc_it(int s){
  IntTab *res = malloc(sizeof(IntTab));
  res->size = s;
  res->tab = (long*) malloc(sizeof(long)*s);
  return res;
}

void
free_it(IntTab* p){
  free(p->tab);
  free(p);
}

void
put_it(int n, long q, IntTab* p){
  p->tab[n]= q;
}

long
get_it(int n, IntTab* p){
  return p->tab[n];
}

void
finalise_it(value v){
  IntTab*p =(IntTab*) Field(v,1); /* */
  int i;
  printf ("reclamation of an IntTab by finalization [");
  for (i=0; i< p->size; ++i){
    printf("%ld ", p->tab[i]);
  }
  printf("]\n");
  fflush(stdout);
  free_it((IntTab*)(Field(v,1)));
}

value
create(value s){
  value block;
  block = caml_alloc_final(2,finalise_it, Int_val(s)*sizeof(IntTab),10000);
  Field(block,1)=(value)alloc_it(Int_val(s));
  return block;
}


value
put(value n, value q, value t){
  put_it(Int_val(n),Long_val(q),(IntTab*)Field(t,1));
  return Val_unit;
}

value
get(value n, value t){
  long res = get_it(Int_val(n),(IntTab*)Field(t,1));
  return Val_long(res);
}
  

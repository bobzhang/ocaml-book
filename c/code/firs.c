
/* @(#)firs.c
 */

#include <stdio.h>
#include <caml/mlvalues.h>


value
plus_native(value x1, value x2, value x3, value x4, value x5, value x6){
  printf ("<< Native Plus >>\n");
  fflush(stdout); /* flush is wrong, why not check it? */
  return Val_long(Long_val(x1)+Long_val(x2)+Long_val(x3)+
		  Long_val(x4)+Long_val(x5)+Long_val(x6));
}

/* value  */
/* plus_bytecode(value * tab_val, int num_val){ */
/*   int i ; */
/*   long res; */
/*   printf("<< Bytecode Plus>>\n"); */
/*   fflush(stdout); */
/*   for (i=0; i<num_val; ++i){ */
/*     printf("%d",i); */
/*     res+=Long_val(tab_val[i]); */
/*   } */
/*   return res; */
/* } */

value
plus_bytecode(value * tab_val, int num_val){
  printf("<< Byte Plus >>\n");
  fflush(stdout);
  return plus_native(tab_val[0],tab_val[1],tab_val[2],
		     tab_val[3],tab_val[4],tab_val[5]);
}



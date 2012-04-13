/* -*- Mode:c; -*-
   *===----------------------------------------------------------------------===
   * Version: $Id: plus_stubs.c,v 0.0 2012/02/22 04:41:52 bobzhang1988 Exp $
   *===----------------------------------------------------------------------===*/


#include <stdio.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <assert.h>
#include <unistd.h>
#include <stdbool.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <sys/wait.h>
#include <pwd.h>
#include <grp.h>
#include <signal.h>
#include <setjmp.h>
#include <caml/mlvalues.h>

/* notice the first is a poiter */
value
plus_six_bc(value* ar, value n){
  int i=0;
  long res =0;
  for(; i< n ;++i){
    res+=Long_val(ar[i]);
  }
  return Val_long(res);
}

value
plus_six_nc(value i1, value i2, value i3,
	    value i4, value i5, value i6){
  return Val_long(Long_val(i1)+ Long_val(i2) + Long_val(i3)+
		  Long_val(i4) + Long_val(i5) + Long_val(i6));
}





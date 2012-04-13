/* -*- Mode:C; -*-
   *===----------------------------------------------------------------------===
   * Version: $Id: inspect_stubs.c,v 0.0 2012/02/22 22:30:16 bobzhang1988 Exp $
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

void margin(int n){
  while(n-->0){printf(".");}
}

value
explore_string(value s){
  char* st = String_val(s) ;
  int len = Bosize_val(s);
  int i=0;
  int code ; 
  for(i=0;i<len;++i){
    code = (int) st[i];
    if(code>31 && code <128 ){
      putchar((char)code);
    }
    else {
      printf("(#%d)",code);
    }
  }
  return Val_unit;
}

void print_block(value v, int m){
  margin(m);printf("\n");
  int size, i,tag;
  if(Is_long(v)){
    margin(m);
    printf("immediate value\n");
    margin(m);
    printf("v as c integer: (%ld); as ocaml int: (%ld);"
	   "as ocaml char: (%c)\n", (long)v, (Long_val(v)), (char)(Long_val(v)));
    return;
  }
  else {
    size = Wosize_val(v);
    tag = Tag_val(v);
    margin(m);
    printf("memory block: size=%d --\n", size);
    switch (tag){
    case Closure_tag:
      margin(m);
      printf("closure with %d closure variables\n", size-1);

      margin(m+4);
      printf("code pointer: %p\n",Code_val(v)); // Field 0
      for(i=1;i<size;++i){
	print_block(Field(v,i),(m+4)); // 
      }
      break;
    case String_tag:
      margin(m);
      printf("string: as c string: %s as ocaml string: "
	     /* (char*)v, (String_val(v))); */
	     // String_val just do pointer arithmetic, does not keep the semantics of ocaml
	     ,(char*)v);
      explore_string(v);
      printf("\n");
      break;
    case Double_tag:
      margin(m);printf("float: %g \n", Double_val(v));
      break;
    case Double_array_tag:
      margin(m);printf("float array:\n");
      for(i=0;i<size/Double_wosize;++i){ // for 64 bit Double_wosize=1
	margin(m);printf(" %g",Double_field(v,i));
      }    break;
    case Abstract_tag:
      margin(m);printf("Not traced by GC tag \n");
      break;
    case Custom_tag:
      margin(m);printf("Custom tag\n");
      break;
    default:
      if(tag>=No_scan_tag){margin(m);printf("No_scan_tag or above\n"); break;}
      else{
	margin(m);
	printf("unknown structured block (tag=%d):\n", Tag_val(v));
	for(i=0;i<size;i++){print_block(Field(v,i),m+4);}
	break;
      }
    }
    return;
  }
}

value
inspect(value v){
  print_block(v,0);
  fflush(stdout);
  return v;
}
		 


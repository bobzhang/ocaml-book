/* -*- Mode:C; -*-
   *===----------------------------------------------------------------------===
   * Version: $Id: basic_stubs.c,v 0.0 2012/02/23 04:07:20 bobzhang1988 Exp $
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
#include <caml/alloc.h>
#include <caml/memory.h>

value
get_an_int(value v){
  int i;
  i=Int_val(v);
  printf("%d\n",i);
  fflush(stdout);
  return Val_unit;
}

value
get_a_float(value v){
  double f;
  f = Double_val(v);
  printf("%lf\n",f);
  fflush(stdout);
  return Val_unit;
}

value
get_a_string(value v){
  char * s= String_val(v);
  printf("%s\n",s);
  fflush(stdout);
  return Val_unit;
  
}


value
send_an_int(value _){
  int i=5;
  printf("%d\n",i);
  fflush(stdout);
  return Val_int(i);
}

value
send_a_float(value _){
  double f = 3.0;
  printf("%lf\n",f);
  return caml_copy_double(f); //
}

value
send_a_string(value _){
  char * t = "gshg\0oshgo";
  printf("%s",t);
  fflush(stdout);
  return caml_copy_string(t); // caml_copy_string respect '\0'
}


/* Int, Double, String
   Generic Function
 */
value
inspect_tuple(value abc){

  printf("%d\n",Int_val(Field(abc,0)));
  printf("%lf\n", Double_val(Field(abc,1)));
  printf("%s\n", String_val(Field(abc,2)));
  fflush(stdout);
  return Val_unit;
}


/*
 * Field -> value *
 * Double_field -> double *
 */
value
inspect_int_array(value bs){
  int len = Wosize_val(bs);
  int i = 0;
  /* for(;i<len;++i){ */
  /*   printf("\d,",Int_val(Field(bs,i))); // expensive mult each time */
  /* } */

  /* int * s = (int*) bs; */
  value * s = (value*)bs;
  for(;i<len;++i){
    printf("%d,", Int_val(*(s++)));
  }
  printf("\n");
  fflush(stdout);
  return Val_unit;
}

value
inspect_float_array(value bs){
  int len = Wosize_val(bs) /  Double_wosize;
  printf("%d\n",len);
  double * s = (double *) bs;
  int i=0;
  // Double_field(bs,i)
  for(;i<len;++i){
    printf("%lf,", Double_val(s++));
    // Double_val needs a pointer actually
  }
  printf("\n");
  fflush(stdout);
  return Val_unit;
}

value
inspect_string_list(value ls){
  CAMLparam1(ls); // for safety when allocation exist 
  CAMLlocal1(cur); // for safety
  cur=ls;
  while(cur!= Val_emptylist){
    printf("%s\n",String_val(Field(cur,0)));
    cur= Field(cur,1);
  }
  CAMLreturn(Val_unit); // for safety
}



/*
 * 
 */
value
create_tuple(value a, value b, value c){
  CAMLparam3(a,b,c);
  CAMLlocal1(tmp);
  tmp=caml_alloc(3,0); // allocate 3 tag 0 on ocaml heap

  Store_field(tmp,1,a);
  Store_field(tmp,2,a);
  Store_field(tmp,3,a);

  CAMLreturn(tmp);
}

/*
 * string -> char list 
 */
value
create_list(value ls){
  CAMLparam1(ls);
  CAMLlocal2(tmp,prev);

  char * s = String_val(ls); // String stores the raw data
  printf("%s\n",s); // here you can see it's exactly the same as c string 
  
  prev = Val_emptylist;
  int len = caml_string_length(ls);
  int i= len - 1;
  for(;i>=0 ;--i){
    tmp=caml_alloc(2,0);
    Store_field(tmp,0, (Val_int( s[i]))); // so you need to transform to Val
    Store_field(tmp,1, prev);
    prev=tmp;
  }
  CAMLreturn(tmp); 
} 


value
create_float_array(value len,value init){
  CAMLparam1(len);
  CAMLlocal1(arr);
  int i=0;
  int l = Int_val(len);
  double tmp = Double_val(init);

  printf("%d\n",l);
  printf("%g\n",tmp);
  
  arr = caml_alloc( l *  Double_wosize,Double_array_tag);

  for(; i<l ;++i){
    Store_double_field(arr,i,tmp);
   // unlike Store_file, we here accept double instead of value
  }
  CAMLreturn(arr);
}

typedef enum _moving{
  WALKING,
  RUNNING,
  SWIMMING,
  FLYING,
}moving;

value
send_an_enum(value v){
  int c = Int_val(v);

  switch(c){
  case 0: puts("Walking");break;
  case 1: puts("Running");break;
  case 2: puts("Swimming");break;
  case 3: puts("Flying");break;
  default: puts("error");break;
  }
  return Val_unit;
}


static const moving table_moving[]={
  WALKING,
  RUNNING,
  SWIMMING,
  FLYING,
};

/*
 * ocaml int -> ocaml enum
 */
value get_an_enum(value v){
  moving res = table_moving[Long_val(v)];
  switch(res){
  case WALKING: puts("Walking");break;
  case RUNNING: puts("Running");break;
  case SWIMMING: puts("Swimming");break;
  case FLYING: puts("Flying");break;
  default: puts("error");break;
  }
  return Val_unit;
}




#define ShiftMask      (1<<0)
#define LockMask       (1<<1)
#define ControlMask    (1<<2)
#define Mod1Mask       (1<<3)

/*
  type state =
  | ShiftMask
  | LockMask
  | ControlMask
  | Mod1Mask

  type state_mask = state list
*/

static const int state_mask_table[]={
  ShiftMask,
  LockMask,
  ControlMask,
  Mod1Mask,
};

static inline int
state_mask_val(value mask_list){
  value cur = mask_list;
  int res = 0;
  while(cur != Val_emptylist){
    res |= state_mask_table[Int_val(Field(cur,0))];
    cur = Field(cur,1);
  }
  return res;
}

#define Val_ShiftMask    Val_int(0)
#define Val_LockMask     Val_int(1)
#define Val_ControlMask  Val_int(2)
#define Val_Mod1Mask     Val_int(3)



static value
Val_state_mask(int c_mask){
  CAMLparam0();
  CAMLlocal2(prev,cons);
  prev = Val_emptylist;

  if(c_mask&ShiftMask){
    cons = caml_alloc(2,0);
    Store_field(cons,0,Val_ShiftMask);
    Store_field(cons,1,prev);
    prev=cons;
  }

  if(c_mask&LockMask){
    cons = caml_alloc(2,0);
    Store_field(cons,0,Val_LockMask);
    Store_field(cons,1,prev);
    prev=cons;
  }

  if(c_mask&ControlMask){
    cons = caml_alloc(2,0);
    Store_field(cons,0,Val_ControlMask);
    Store_field(cons,1,prev);
    prev=cons;
  }

  if(c_mask&Mod1Mask){
    cons = caml_alloc(2,0);
    Store_field(cons,0,Val_Mod1Mask);
    Store_field(cons,1,prev);
    prev=cons;
  }

  CAMLreturn(prev);
}

/*
 * Exchaning Raw Data
 * ocaml strings wherever you have array of bytes or void*
 * chose a bigarray for matrix 
 */

value
get_raw_data(value _){
  CAMLparam0();
  CAMLlocal1(ml_data);
  char * raw_data;
  int data_len=20;
  raw_data= malloc(data_len);
  strncpy(raw_data,"get_raw_data",data_len);
  
  ml_data= caml_alloc_string(data_len);
  memcpy(String_val(ml_data),raw_data,data_len);
  free(raw_data);

  CAMLreturn(ml_data);
}




/*
 * Maybe you make use of ocaml as a memory pool..
 */
value
get_raw_data2(value _){
  CAMLparam0();
  CAMLlocal1(ml_data);
  char * raw_data = NULL;
  ml_data= caml_alloc_string(20); // raw data allocation
  raw_data = String_val(ml_data);
  strncpy(raw_data,"gsogho ha",20);
  // Byte(ml_data,i)
  // Byte_u(ml_data,i)
  CAMLreturn (ml_data);
}

/*
 * caml_invalid_argument("Error msg");
 * caml_failwith("Error msg");
 */

typedef struct _obj_st{
  double d;
  int i;
  char c;
}obj_st;

typedef obj_st *obj_p;


value
wrapping_ptr_ml2c(value d, value i, value c){
  obj_p  my_obj = malloc(sizeof(obj_st));
  my_obj -> d = Double_val(d);
  my_obj -> i = Int_val(i);
  my_obj -> c = Int_val(c);
  return (value) my_obj;
}

value
dump_ptr(value ml_ptr){
  obj_p my_obj = (obj_p) ml_ptr;
  printf(" d: %g\n i: %d\n c: %c\n",
	 my_obj->d,
	 my_obj->i,
	 my_obj->c
    );
  return Val_unit;
}

value
free_ptr(value ml_ptr){
  free((obj_p)ml_ptr);
  return Val_unit;
}

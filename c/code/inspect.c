#include<stdio.h>
#include<caml/mlvalues.h>

value
inspect(value v){
  if(Is_long(v)){
    printf ("v is an integer (%ld): %ld\n",(long) v,Long_val(v));
  }
  else if (Is_block(v)){
    printf ("v is a pointer\n");
  }
  else {
    printf ("v is neither an integer nor a pointer ???\n");
  }
  fflush(stdout);
  return v;
}

void
margin (int n){
  while(n-- > 0){
    printf(".");
  }
  return ;
}


void
print_block(value v, int m){
  int size, i;
  margin(m);
  if (Is_long(v)){
    printf ("immediate value (%ld)\n\n",Long_val(v));
    return;
  }
  printf ("memory block: size = %d - \n",size=Wosize_val(v));
  switch(Tag_val(v)){
  case Closure_tag:
    printf ("closure with %d free variables \n\n",size-1);
    margin(m+4);
    printf ("code pointer%p\n",Code_val(v));
    for (i=1; i < size; ++i){
      print_block(Field(v,i),m+4); /* traverse */
    };
    break;
  case String_tag:
    printf("string: %s (%s)\n", String_val(v), (char*) v);
    break;
  case Double_tag:
    printf("float: %g\n", Double_val(v));
    break;
  case Double_array_tag:
    printf ("float array:");
    for (i=0; i < size/Double_wosize; ++i){
      printf("%g", Double_field(v,i));
    };
    break;
  case Custom_tag:
    printf("custom tag:\n"); break; /* finalized function */
  case Abstract_tag: printf("abstract type\n");break;
  /* case Final_tag: printf ("abstract finalized type\n");break; */
  default:
    if(Tag_val(v)>=No_scan_tag){
      printf ("unknown tag\n");
      break;
    }
    for(i=0;i<size;i++){
      print_block(Field(v,i),m+4);
    }
    return;

  }
}

value
inspect_block(value v){
   print_block(v,0);
   fflush(stdout);
   return v;
}


void /* padding will also be dumped */
explore_string(value v){
  char *s;
  int i,size;
  s=(char*)v;
  size=Wosize_val(v)*sizeof(value);
  for (i=0; i < size; ++i){
    int p = (unsigned int) s[i];
    if((p>31)&& (p<128)){
      printf ("%c",s[i]);
    }
    else {
      printf ("(#%u)",p);
    }
  }
  printf("\n");
  fflush(stdout);
}


void 
swap_char(value v, int i, int j){
  char c = Byte(v,i);
  Byte(v,i)  = Byte (v,j);
  Byte(v,j) = c;
}

value
swap_string(value v){
  int i , j, t = caml_string_length(v);
  for (i=0, j = t - 1; i < t/2; ++i,--j){
    swap_char(v,i,j);
  };
  return v;
}



value make_str(char *s){
  return caml_copy_string(s);
}
value make_str_array(char ** p){
  return caml_alloc_array(make_str,p);
}

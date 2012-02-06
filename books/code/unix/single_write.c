
/* @(#)single_write.c
 */
#include <errno.h>
#include <string.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include <caml/unixsupport.h>

CAMLprim value
caml_single_write (value fd, value buf, value vofs, value vlen){
  CAMLparam4(fd,buf,vofs,vlen);
  /*
    During the _system_call_, GC may move buf, so we copy it first
    before _system_call_
   */
  long ofs, len;
  int numbytes,ret;
  char iobuf[UNIX_BUFFER_SIZE];
  ofs = Long_val(vofs);
  len = Long_val(vlen);
  ret = 0;
  if(len>0){
    numbytes = len > UNIX_BUFFER_SIZE ? UNIX_BUFFER_SIZE : len;
    memmove(iobuf, &Byte(buf,ofs), numbytes);
    caml_enter_blocking_section(); /* release Global Lock */
    ret = write(Int_val(fd), iobuf, numbytes);
    caml_leave_blocking_section(); /* reaquire Global Lock */
    if(ret==-1)uerror("single_write", Nothing);
  }
  CAMLreturn((Val_int(ret)));
}

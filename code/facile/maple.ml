open Format
open Facile 
open Easy 

let _ =
  let red = 0 and white = 1 in
  let dom = Domain.create [red; white] in
  let l = Fd.create dom and c = Fd.create dom
  and r = Fd.create dom and m = Fd.create dom in

  (* Constraints *)
  Cstr.post (fd2e l <>~  fd2e c);
  Cstr.post (fd2e c <>~  fd2e r);
  Cstr.post (fd2e m <>~  fd2e c);
  Cstr.post (fd2e m =~  i2e red);

  (* Goal *)
  let var_list = [l;c;r;m] in
  let goal = Goals.List.labeling var_list in

  (* Search *)
  if Goals.solve goal then begin
    printf "l=@."; Fd.fprint stdout l;
    printf "c=@."; Fd.fprint stdout c;
    printf "r=@."; Fd.fprint stdout r;
    printf "m=@."; Fd.fprint stdout m;
    print_newline () end
  else
    prerr_endline "No solution"


















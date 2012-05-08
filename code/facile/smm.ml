open Format
open Facile
open Easy

let _ =
  (* Variables *)
  let s = Fd.interval 0 9 and e = Fd.interval 0 9 and n = Fd.interval 0 9
  and d = Fd.interval 0 9 and m = Fd.interval 0 9 and o = Fd.interval 0 9
  and r = Fd.interval 0 9 and y = Fd.interval 0 9 in
  (* Constraints *)
  Cstr.post (fd2e m >~  i2e 0);
  Cstr.post (fd2e s >~  i2e 0);
  let digits = [|s;e;n;d;m;o;r;y|] in
  
  Cstr.post (Alldiff.cstr digits);

  let c = Fd.array 3 0 1 in (* Carry array *)

  let one x = fd2e x and ten x = i2e 10 *~  fd2e x in begin 

    Cstr.post ( one d +~  one e =~  one y +~  ten c.(0));
    Cstr.post (one c.(0) +~  one n +~  one r =~  one e +~  ten c.(1));
    Cstr.post (one c.(1) +~  one e +~  one o =~  one n +~  ten c.(2));
    Cstr.post (one c.(2) +~  one s +~  one m =~  one o +~  ten m);

    (* Search goal solving *)
    if Goals.solve (Goals.Array.labeling digits) then begin
      let value = Fd.elt_value in begin
        Array.iter (Fd.fprint stdout) digits; 
        prerr_endline "succeed";
      end 
    end else
      prerr_endline "No solution"
  end




















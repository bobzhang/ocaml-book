open BatPervasives
open Printf

let _ = Unix.(begin
  ignore Sys.(signal sighup Signal_ignore);
  (** The exe system call sets all the behaviors to Signal_default
      except that signals ignored before are still ignored
      afterward
  *)
  execvp Sys.argv.(1) (Array.sub Sys.argv 1 (Array.length Sys.argv - 1 ));
end 
)




















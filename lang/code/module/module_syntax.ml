module type DEVICE = sig end
  
let devices: (string, (module DEVICE)) Hashtbl.t = Hashtbl.create 18
(* val devices : (string, (module DEVICE)) Batteries.Hashtbl.t = <abstr> *)
module PDF = struct end
(* module PDF : sig  end *)
let _ = Hashtbl.add devices "PDF" (module PDF : DEVICE)

module Device =
  (val (Hashtbl.find devices "PDF") : DEVICE)
(* module Device : DEVICE *)








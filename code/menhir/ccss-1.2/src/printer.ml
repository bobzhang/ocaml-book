(********************************************************************************)
(*	Printer.ml
	Copyright (c) 2010 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open Printf


(********************************************************************************)
(**	{1 Exceptions}								*)
(********************************************************************************)

exception Variable_redeclared of Lexing.position * string
exception Variable_undeclared of Lexing.position * string
exception Invalid_arithmetic of Lexing.position * string
exception Invalid_units of Lexing.position * string * string * string


(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

type operation_t =
	| Addition
	| Subtraction
	| Multiplication
	| Division


type calcres_t =
	| Numeric of float * string option
	| Alpha of string


(********************************************************************************)
(**	{1 Functions and values}						*)
(********************************************************************************)

(********************************************************************************)
(**	{2 Auxiliary functions}							*)
(********************************************************************************)

let sprint_list ?(filter = false) ?(termin = "") ?(sep = termin) f xs =
	let rec concat = function
		| []	   -> ""
		| [x]	   -> x ^ termin
		| hd :: tl -> hd ^ sep ^ (concat tl) in
	let lst = List.map f xs in
	let lst' = if filter then List.filter (fun x -> x <> "") lst else lst
	in concat lst'


let func_of_op = function
	| Addition	 -> ( +. )
	| Subtraction	 -> ( -. )
	| Multiplication -> ( *. )
	| Division	 -> ( /. )


let string_of_op = function
	| Addition	 -> "add"
	| Subtraction	 -> "subtract"
	| Multiplication -> "multiply"
	| Division	 -> "divide"


let string_of_unit = function
	| Some u -> "'" ^ u ^ "'"
	| None	 -> "a scalar"


let find_category =
	let units = Hashtbl.create 14 in
	let () =
		(* The base unit for the `Length category is mm *)
		Hashtbl.add units "mm" (`Length, 1.0);
		Hashtbl.add units "cm" (`Length, 10.0);
		Hashtbl.add units "in" (`Length, 25.4);
		Hashtbl.add units "pt" (`Length, 0.352778);
		Hashtbl.add units "pc" (`Length, 4.23333);

		(* The base unit for the `Angle category is deg *)
		Hashtbl.add units "deg" (`Angle, 1.0);
		Hashtbl.add units "grad" (`Angle, 0.9);
		Hashtbl.add units "rad" (`Angle, 57.2958);

		(* The base unit for the `Time category is ms *)
		Hashtbl.add units "ms" (`Time, 1.0);
		Hashtbl.add units "s" (`Time, 1000.0);

		(* The base unit for the `Frequency category is hz *)
		Hashtbl.add units "hz" (`Frequency, 1.0);
		Hashtbl.add units "Hz" (`Frequency, 1.0);
		Hashtbl.add units "khz" (`Frequency, 1000.0);
		Hashtbl.add units "kHz" (`Frequency, 1000.0);
	in function
		| None ->
			None
		| Some u ->
			try Some (Hashtbl.find units u)
			with Not_found -> None
		

let normalise_units pos op (num1, u1) (num2, u2) = match (find_category u1, find_category u2) with
	| (Some (cat1, fact1), Some (cat2, fact2)) when cat1 = cat2 ->
		let num2' = (num2 *. fact2) /. fact1
		in (num1, num2', u1)
	| _ ->
		raise (Invalid_units (pos, string_of_op op, string_of_unit u1, string_of_unit u2))


(********************************************************************************)
(**	{2 Main printing functions}						*)
(********************************************************************************)

let sprint convert stylesheet =

	let variables = Hashtbl.create 32 in

	let rec sprint_stylesheet (maybe_charset, statements) =
		let str1 = match maybe_charset with
			| Some charset -> sprintf "@charset \"%s\"" charset
			| None	       -> ""
		and str2 = sprint_list ~filter:true ~termin:"\n\n" sprint_statement statements
		in str1 ^ str2

	and sprint_statement = function
		| `Import (source, maybe_media) ->
			sprintf "@import %s%s;"
				(sprint_source source)
				(match maybe_media with None -> "" | Some media -> " " ^ (sprint_media media))
		| `Media (media, rules) ->
			sprintf "@media %s\n{\n%s}"
				(sprint_media media)
				(sprint_list ~termin:"\n" sprint_rule rules)
		| `Page (pseudo_page, declarations) ->
			sprintf "@page %s\n\t{\n%s\t}"
				(match pseudo_page with None -> "" | Some x -> ":" ^ x)
				(sprint_list ~termin:"\n" sprint_declaration declarations)
		| `Fontface declarations ->
			sprintf "@font-face\n\t{\n%s\t}"
				(sprint_list ~termin:"\n" sprint_declaration declarations)
		| `Vardecl (pos, id, expression) ->
			if Hashtbl.mem variables id
			then raise (Variable_redeclared (pos, id))
			else Hashtbl.add variables id expression; ""
		| `Rule rule ->
			sprint_rule rule

	and sprint_source = function
		| `String s -> sprintf "\"%s\"" s
		| `Uri s    -> sprintf "url(\"%s\")" s

	and sprint_media media =
		sprint_list ~sep:", " (fun x -> x) media

	and sprint_rule (selectors, declarations) =
		sprintf "%s\n\t{\n%s\t}" (sprint_list ~sep:", " sprint_selector selectors) (sprint_list ~termin:"\n" sprint_declaration declarations)

	and sprint_selector (simplesel, combinations) =
		(sprint_simplesel simplesel) ^ (sprint_list sprint_combination combinations)

	and sprint_simplesel = function
		| `Explicit (el, qs) -> (sprint_element el) ^ (sprint_list sprint_qualifier qs)
		| `Generic (hd, tl)  -> (sprint_list sprint_qualifier (hd :: tl))

	and sprint_combination (combinator, simplesel) =
		(sprint_combinator combinator) ^ (sprint_simplesel simplesel)

	and sprint_combinator = function
		| `Descendant	    -> " "
		| `General_sibling  -> " ~ "
		| `Adjacent_sibling -> " + "
		| `Child	    -> " > "

	and sprint_element = function
		| `Tag s     -> s
		| `Universal -> "*"

	and sprint_qualifier = function
		| `Id str		-> "#" ^ str
		| `Class str		-> "." ^ str
		| `Attr (name, value)	-> sprintf "[%s%s]" name (sprint_attr value)
		| `Pseudo_class str	-> ":" ^ str
		| `Pseudo_element str	-> "::" ^ str
		| `Sel_func (str, args) -> sprintf ":%s(%s)" str (sprint_function args)

	and sprint_function = function
		| `Qualified qualifiers -> sprint_list sprint_qualifier qualifiers
		| `Nth str		-> str

	and sprint_attr = function
		| `Attr_exists	    -> ""
		| `Attr_equals v    -> "=" ^ "\"" ^ v ^ "\""
		| `Attr_includes v  -> "~=" ^ "\"" ^ v ^ "\""
		| `Attr_dashmatch v -> "|=" ^ "\"" ^ v ^ "\""
		| `Attr_prefix v    -> "^=" ^ "\"" ^ v ^ "\""
		| `Attr_suffix v    -> "$=" ^ "\"" ^ v ^ "\""
		| `Attr_substring v -> "*=" ^ "\"" ^ v ^ "\""

	and sprint_declaration (property, expression, important) =
		sprintf "\t%s: %s%s;" property (sprint_expression expression) (if important then " !important" else "")

	and sprint_expression expression =
		sprint_list ~sep:", " sprint_sentence expression

	and sprint_sentence sentence =
		sprint_list ~sep:" " sprint_term sentence

	and sprint_term = function
		| `Calc calc		-> sprint_calc calc
		| `String str		-> sprintf "\"%s\"" str
		| `Ident str		-> str
		| `Uri str		-> sprintf "url(\"%s\")" str
		| `Hash str		-> "#" ^ str
		| `Term_func (f, expr)	-> sprintf "%s(%s)" f (sprint_expression expr)
		| `Slash		-> "/"

	and sprint_calc calc = match expand_calc calc with
		| Numeric (num, units) -> (sprintf "%.4g" num) ^ (match units with Some s -> s | None -> "")
		| Alpha str	       -> str

	and expand_calc = function
		| `Varref (pos, id) ->
			begin
				try
					match Hashtbl.find variables id with
						| [[`Calc calc]] -> expand_calc calc
						| expression -> Alpha (sprint_expression expression)
				with
					Not_found -> raise (Variable_undeclared (pos, id))
			end
		| `Quantity (num, units) ->
			Numeric (num, units)
		| `Sum (pos, c1, c2) ->
			perform_calc Addition pos c1 c2
		| `Sub (pos, c1, c2) ->
			perform_calc Subtraction pos c1 c2
		| `Mul (pos, c1, c2) ->
			perform_calc Multiplication pos c1 c2
		| `Div (pos, c1, c2) ->
			perform_calc Division pos c1 c2

	and perform_calc op pos x y =
		let func = func_of_op op
		and (num1, units1) = match expand_calc x with
			| Numeric (num, units) -> (num, units)
			| _		       -> raise (Invalid_arithmetic (pos, string_of_op op))
		and (num2, units2) = match expand_calc y with
			| Numeric (num, units) -> (num, units)
			| _		       -> raise (Invalid_arithmetic (pos, string_of_op op)) in
		let (num1', num2', units) = match (op, units1, units2) with
			| (Addition, u1, u2) when u1 = u2	  -> (num1, num2, u1)
			| (Addition, u1, None) when num2 = 0.0	  -> (num1, num2, u1)
			| (Addition, None, u2) when num1 = 0.0	  -> (num1, num2, u2)
			| (Addition, u1, u2) when convert	  -> normalise_units pos op (num1, u1) (num2, u2)
			| (Subtraction, u1, u2)	when u1 = u2	  -> (num1, num2, u1)
			| (Subtraction, u1, None) when num2 = 0.0 -> (num1, num2, u1)
			| (Subtraction, None, u2) when num1 = 0.0 -> (num1, num2, u2)
			| (Subtraction, u1, u2)	when convert	  -> normalise_units pos op (num1, u1) (num2, u2)
			| (Multiplication, None, u2)		  -> (num1, num2, u2)
			| (Multiplication, u1, None)		  -> (num1, num2, u1)
			| (Division, u1, u2) when u1 = u2	  -> (num1, num2, None)
			| (Division, u1, None)			  -> (num1, num2, u1)
			| (op, u1, u2)				  -> raise (Invalid_units (pos, string_of_op op, string_of_unit u1, string_of_unit u2))
		in Numeric (func num1' num2', units)

	in sprint_stylesheet stylesheet


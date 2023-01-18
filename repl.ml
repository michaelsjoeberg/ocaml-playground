(* https://bernsteinbear.com/blog/lisp/00_fundamentals/ *)

type stream = { mutable line_num: int; mutable chr: char list; chan: in_channel };;

let read_char stm =
	match stm.chr with
	| [] ->
		let c = input_char stm.chan in
		if c = '\n' then let _ = stm.line_num <- stm.line_num + 1 in c
		else c
	| c::rest ->
		let _ = stm.chr <- rest in c


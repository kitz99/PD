open Lexing
open Types

let rec init_mem v = function
  | [] -> []
  | h::t -> (h,v)::(init_mem v t)

let _ =
   let cin =
      if Array.length Sys.argv > 1
      then open_in Sys.argv.(1)
      else failwith "please specify program file"
  in
    let lexbuf = Lexing.from_channel cin in
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = Sys.argv.(1) };
      let pgm = Parser.main Lexer.token lexbuf in
          let locs = ImpAST.locations pgm in
          if (type_check  (init_mem TIntRef locs) pgm) then
             print_string (Semantics.string_of_config (Semantics.evaluate (pgm,init_mem 0 locs)
))
          else ()

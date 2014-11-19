(* File lexer.mll *)
{
open Parser        (* The type token is defined in parser.mli *)

let incr_linenum lexbuf =
      let pos = lexbuf.Lexing.lex_curr_p in
      lexbuf.Lexing.lex_curr_p <- { pos with
        Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
        Lexing.pos_bol = pos.Lexing.pos_cnum;
      }
}
rule token = parse
    [ '\n' ] { incr_linenum lexbuf ; token lexbuf }
  | [' ' '\t' '\r' ]     { token lexbuf }     (* skip blanks *)
  | ['-']?['0'-'9']+ as lxm { INT(int_of_string lxm) }
  | '+'            { PLUS }
  | '*'            { MUL }
  | "<="           { LTE }
  | '('            { LPAREN }
  | ')'            { RPAREN }
  | ":="           { ASGNOP }
  | ';'            { SEQ }
  | "if"           { IF }
  | "then"         { THEN }
  | "else"         { ELSE }
  | "while"        { WHILE }
  | "do"           { DO }
  | "done"         { DONE }
  | "true"         { TRUE }
  | "false"        { FALSE }
  | "skip"         { SKIP }
  | '!'            { DEREF }
  | ['A'-'Z' 'a'-'z'] ['A'-'Z' 'a'-'z' '0'-'9' '_'] * as id
                   { LOC(id)} 
  | eof            { EOF }

/* File parser.mly */
%{
open ImpAST
open Lexing

let location () =  let start_pos = Parsing.symbol_start_pos () in
    let end_pos = Parsing.symbol_end_pos () in
    Printf.sprintf "%s:%d.%d-%d.%d"
      start_pos.pos_fname
      start_pos.pos_lnum (start_pos.pos_cnum - start_pos.pos_bol)
      end_pos.pos_lnum (end_pos.pos_cnum - end_pos.pos_bol)

%}
%token <int> INT
%token <string> LOC
%token TRUE FALSE
%token SEQ SKIP
%token IF THEN ELSE
%token WHILE DO DONE
%token LTE
%token ASGNOP DEREF
%token MUL
%token DIV
%token PLUS
%token MIN
%token LPAREN RPAREN
%token EOF
%right SEQ
%nonassoc LTE
%right ASGNOP
%left PLUS /* lowest precedence */
%left MIN
%left MUL
%left DIV
%nonassoc DEREF       /* highest precedence */
%start main             /* the entry point */
%type <ImpAST.expr> main
%%
main:
    expr EOF                { $1 }
;
expr:
    INT                         { Int ($1,location()) }
  | TRUE                        { Bool (true, location()) }
  | FALSE                       { Bool (false, location()) }
  | SKIP                        { Skip (location()) }
  | LPAREN expr RPAREN          { $2 }
  | expr MUL expr               { Op ($1,Mul,$3, location()) }
  | expr DIV expr               { Op ($1,Div,$3, location()) }
  | expr PLUS expr              { Op ($1,Plus,$3, location()) }
  | expr MIN expr               { Op ($1,Min,$3, location()) }
  | DEREF LOC                   { Loc ($2, location()) }
  | LOC ASGNOP expr             { Atrib ($1,$3, location()) }
  | expr LTE expr               { Op ($1, Mic, $3, location()) }
  | expr SEQ expr               { Secv ($1,$3, location()) }
  | IF expr THEN expr ELSE expr { If ($2, $4, $6, location()) }
  | WHILE expr DO expr DONE     { While ($2, $4, location()) }
;

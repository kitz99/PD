open Mem
open ImpAST

let rec reduce = function
  | (Op(Int (n1,_),Mul,Int (n2,_),loc),s) -> (Int (n1*n2,loc),s)             (*Op* *) 
  | (Op(Int (n1,_),Div,Int (n2,_),loc),s) -> (Int (n1/n2,loc),s)             (*Op* *)
  | (Op(Int (n1,_),Plus,Int (n2,_),loc),s) -> (Int (n1+n2,loc),s)             (*Op+*)
  | (Op(Int (n1,_),Min,Int (n2,_),loc),s) -> (Int (n1-n2,loc),s)             (*Op+*)
  | (Op(Int (n1,_),Mic,Int (n2,_),loc),s) -> (Bool (n1<=n2,loc),s)            (*Op<=*)
  | (Op(Int (n1,loc1),op,e2,loc),s) ->                                        (*OpD*)
    (match reduce (e2,s) with (e2',s') -> (Op(Int (n1,loc1),op,e2',loc),s'))
  | (Op(e1,op,e2,loc),s) ->                                            (*OpS*)
    (match reduce (e1,s) with (e1',s') -> (Op(e1',op,e2,loc),s'))
  | (Loc (l,loc), s) -> (Int (lookup l s,loc), s)                    (*Loc*)
  | (Atrib(l, Int (n,_),loc),s) ->                                         (*Atrib*)
      (Skip loc, update (l,n) s)
  | (Atrib(l,e,loc),s) ->                                          (*AtribD*)
    (match reduce (e,s) with (e',s') -> (Atrib(l,e',loc),s'))
  | (Secv(Skip _,e,_),s) -> (e,s)                                 (*Secv*)
  | (Secv(e1,e2,loc),s) ->                                             (*SecvS*)
    (match reduce (e1,s) with (e1',s') -> (Secv(e1',e2,loc),s'))
  | (If(Bool (true,_),e1,e2,_),s) -> (e1,s)                         (*IfTrue*)
  | (If(Bool (false,_),e1,e2,_),s) -> (e2,s)                        (*IfFalse*)
  | (If(e,e1,e2,loc),s) ->                                             (*IfS*)
    (match reduce (e,s) with (e',s') -> (If(e',e1,e2,loc),s'))
  | (While(e1,e2,loc),s) -> (If(e1,Secv(e2,While(e1,e2,loc),loc),Skip loc,loc),s) (*While*)
  | _ -> raise Not_found                                                      (*default*)


(* evaluate basically computes the transitive closure ->* of the
   one step reduction relation. *)
let rec evaluate c = try evaluate (reduce c) with
  | Not_found -> c


let string_of_config (p,m) = "<" ^ string_of_expr p ^ ", {" ^ string_of_mem m ^ "} >"

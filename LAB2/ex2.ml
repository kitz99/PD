(*Functii recursive *)

(*Factorialul *)
let rec fact x = 
       if x <= 1 then 1
       else x * fact (x - 1);;

(* Numerele fibonacci ineficient *)
let rec fib n = 
      if n = 0 then 0
      else 
        if n = 1 then 1
        else fib (n - 1) + fib (n - 2);;

(* Numerele fibonacci eficient *)
let rec fibef f1 f2 n = 
      if n = 0 then f1
      else fibef f2 (f1 + f2) (n - 1);;


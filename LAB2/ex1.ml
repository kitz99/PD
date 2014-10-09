(* Exercitiul 1 *)

let f x = 3 * x;;

let g x = x * x;;

let plus f g = fun x -> (f x) + (g x);;

let o x y = x * y + x - y;;

let lift o f g = fun x -> o((f x) g(x));;

(* Ultimul element al unei liste *)
let rec last = function
	| [] -> failwith("Nu ai elemente")
	| [x] -> x
	| _::l -> last l;; 

(* Ultimele 2 elemente ale listei *)
let rec last2 = function
	| [] -> None
	| [_] -> None
	| [x;y] -> Some(x, y)
	| _::l -> last2 l;;

(* al k-ulea element din lista *)
let rec find k = function
	| [] -> None
	| x :: l -> if k == 0 then Some(x)
	            else find (k-1) l;;

(* number of elements of a list *)
let rec lenght = function
	| [] -> 0
	| x :: l -> 1 + lenght l;;

(* reverse a list*)
let rev l =
	let rec aux lista = function
		| [] -> lista
		| x :: t -> aux(x::lista) t in
	aux [] l;;

(* lista este palindrom *)
let is_pal l = 
	l = rev l;;

(* sortarea unei liste *)

let rec sort = function
	| [] -> []
	| x :: l -> insert x (sort l) 
	and  insert elem = function
		| [] -> [elem]
		| x :: l -> if elem <= x then elem :: x :: l
		             else x :: insert(elem l);;
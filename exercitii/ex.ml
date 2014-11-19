(* Last element *)

let rec last = function
	| [] -> failwith "Fara elemente"
	| [x] -> x
	| x :: l -> last l;;

(* Penultimul element *)
let rec last_but_one = function
	| [] -> failwith "Fara elemente"
	| [x] -> failwith "prea putine elemente"
	| [x;y] -> x
	| _ :: l -> last_but_one l;;

(* al k-ulea elemente in ocaml *)
let rec kath k = function
	| [] -> failwith "Nu exista"
	| x :: l -> if k == 1 then x
	            else kath (k-1) l;;


(*Find the number of elements of a list. (easy)*)
let rec len = function
	| [] -> 0
	| x :: l -> 1 + len l;;

(*Reverse a list *)
let reverse l = 
	let rec aux acc = function
		| [] -> acc
		| x :: t -> aux (x::acc) t
	in aux [] l;;

(* Find out whether a list is a palindrome. (easy) *)
let pal l = 
	l = reverse l;;

(*Concatenarea a 2 liste *)
let concat l1 l2 = 
	l1 @ l2;;

(*o functie care zice daca un element apartine sau nu unei liste*)
let rec apartine k = function
	| [] -> false
	| x :: l -> if x == k  then true
			    else apartine k l;;

(*afla *)
let rec afla k = function
	| [] -> failwith "Lista vida"
	| x :: l -> if fst x == k then snd x
			    else afla k l;; 

(*
	pack ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"];;
	- : string list list =
	[["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"]; ["e"; "e"; "e"; "e"]]
*)
(* pack *)
let taie l = match l with
	| [] -> []
	| a :: x -> x;;
let pack full = 
	let rec aux curent out orig = match orig with
		| [] -> curent::out
		| a :: b -> if List.hd curent = a then aux (a::curent) out b
		            else aux (a::[]) (curent::out) b

	in List.rev(aux [List.hd full] [] (taie full));;

(* Run-length encoding of a list. (easy)
	encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
 *)

let encode l = 
	let rec aux orig elem k out = match orig with
	| [] -> (k, elem)::out
	| a :: b -> if List.hd orig = elem then aux b elem (k+1) out
	            else aux b a 1 ((k, elem) :: out)
	in List.rev(aux l (List.hd l) 0 []);;


(*
	Duplicate the elements of a list. (easy)

	# duplicate ["a";"b";"c";"c";"d"];;
	- : string list = ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"]
*)

let duplicate l = 
	let rec aux orig out = match orig with
		| [] -> out
		| a :: b -> aux b (a :: (a :: out))
	in List.rev(aux l []);;

(*
	# replicate ["a";"b";"c"] 3;;
- : string list = ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"]
*)
let replicate l n = 
	let rec aux orig out k i = match orig with
	| [] -> out
	| a :: b -> if k != i then aux orig (a :: out) k (i + 1)
	            else aux b out k 0
	in List.rev(aux l [] n 0);;

(*
	# drop ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3;;
	- : string list = ["a"; "b"; "d"; "e"; "g"; "h"; "j"]
*)
let drop l n = 
	let rec aux orig out k i = match orig with
	| [] -> out
	| a :: b -> if k = i then aux b out k 1
	            else aux b (a :: out) k (i + 1)
	in List.rev(aux l [] n 1);;

(*
	# split ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3;;
	- : string list * string list =
	(["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"])
	# split ["a";"b";"c";"d"] 5;;
	- : string list * string list = (["a"; "b"; "c"; "d"], [])
*)
let split l n =
	let rec aux orig p1 p2 k i = match orig with
	| [] -> (List.rev p1, List.rev p2)
	| a :: b -> if k != i then aux b (a :: p1) b k (i + 1)
	            else (List.rev(a :: p1), b)
	in aux l [] [] n 1;;

(*
	# slice ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 2 6;;
	- : string list = ["c"; "d"; "e"; "f"; "g"]
*)
let slice l n m = 
	let rec aux orig out k i j = match orig with
	| [] -> out
	| a :: b -> if j >= k && j <= i then aux b (a::out) k i (j+1)
	            else aux b out k i (j+1)
	 in  List.rev(aux l [] n m 0);;

(*
	# rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3;;
	- : string list = ["d"; "e"; "f"; "g"; "h"; "a"; "b"; "c"]
	# rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] (-2);;
	- : string list = ["g"; "h"; "a"; "b"; "c"; "d"; "e"; "f"]
*)
let rotate l n = 
	let rec aux orig out k i = match orig with
	| [] -> out
	| a :: b -> if i <= k then aux b (a :: out) k (i + 1)
	            else orig@List.rev out
	in aux l [] (if n < 0 then len(l) + n else n)  1;; 

(*
	# remove_at 1 ["a";"b";"c";"d"];;
	- : string list = ["a"; "c"; "d"]
*)
let remove_at n l = 
	let rec aux orig out k i = match orig with
	| [] -> List.rev out
	| a :: b ->  if k = i then (List.rev out)@b
	             else aux b (a :: out) k (i+1)
	in aux l [] (if n < 0 || n > len(l) then failwith "Index out of bounds" else n) 0;;

(*
	# insert_at "alfa" 1 ["a";"b";"c";"d"];;
	- : string list = ["a"; "alfa"; "b"; "c"; "d"]
	# insert_at "alfa" 3 ["a";"b";"c";"d"];;
	- : string list = ["a"; "b"; "c"; "alfa"; "d"]
	# insert_at "alfa" 4 ["a";"b";"c";"d"];;
	- : string list = ["a"; "b"; "c"; "d"; "alfa"]
*)
let insert_at x n l = 
	let rec aux orig out k i elem = match orig with
	| [] -> out
	| a :: b -> if k = i then (out @ [elem]) @ orig
                else aux b (out @ [a]) k (i+1) elem
     in aux l [] (if n < 0 || n > len(l) then failwith "index out of bounds" else n) 0 x;;

(*
	# range 4 9;;
	- : int list = [4; 5; 6; 7; 8; 9]
	# range 9 4;;
	- : int list = [9; 8; 7; 6; 5; 4]
*)
let range a b = 
	let rec aux out y i = 
		if i <= y then aux (i :: out) y (i+1)
	    else out
	in if a < b then List.rev(aux [] b a)
	   else aux [] a b;;


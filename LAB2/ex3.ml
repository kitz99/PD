(* exercitii cu liste *)
(* primul element *)
let prim n = 
	match n with
	| a::b -> a
	| [] -> failwith "Error";;

(* ultimul element *)
let rec last = function
    | [] -> failwith "Error"
    | [x] -> x
    | _ :: t -> last t;;

(* penultimul element *)
let rec last1 = function
    | [] -> failwith "Error"
    | [x] -> failwith "Just one element! Please insert at least 2";
    | [x1 ; x2] -> x1
    | _ :: t -> last1 t;;

(* Lungimea listei *)
let rec length = function
	| [] -> 0
	| _ :: t -> 1 + length t;;

(* find *)
let rec find x = function
	| [] -> print_string "false"
	| a :: b -> if a = x then print_string "Am gasit"
	            else  find x b;;

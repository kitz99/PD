type log = True | False | Var of string | And of log * log | Not of log;;

let e1 = And(Not(And(True, False)), Not(False))

let rec string_of_log = function
	| True -> " true "
	| False -> " false "
	| And (x, y) -> "(" ^ string_of_log x ^ " & " ^ string_of_log y ^ ")"
	| Var x -> x
	| Not x -> " !" ^ string_of_log x


let rec simpl = function
	| True -> true
	| False -> false
	| And(x, y) -> (simpl x) && (simpl y)
	| Not x -> not(simpl x)

let simplifica x = 
	match simpl x with
	| true -> True
	| false -> False
	| _ -> failwith "error"

type pereche = {first : string;
                second: log
                }

let rec fnc x = function (* val *)
	| [] -> failwith "Not found"
	| a :: b -> if a.first = x then a.second
	            else  fnc x b;;


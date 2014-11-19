type log = Bool of bool | Not of log | And of log * log | Var of string

let e1 = Not(And(Not (Bool true), Not (Bool false)))

let e2 = And(Not(And(Not (Bool true), Not (Var "x"))), Var "y")

(*1.  Să se scrie o funcție string_of_log care ia ca argument o expresie logică și întoarce un șir de caractere care reprezintă frumos expresia.  Exemple de rulare:*)
let rec string_of_log ex = match ex with
	| Bool true -> "true"
	| Bool false -> "false"
	| Not x -> "!" ^ string_of_log(x)
	| And (a, b) -> "(" ^ string_of_log(a) ^ " & " ^ string_of_log(b) ^ ")"
	| Var x -> x;;

(*2. Să se scrie o funcție eval care  ia ca argument o expresie logică și întoarce valoarea ei de adevăr.  Dacă expresia conține variabile, va eșua.*)
let rec eval ex = match ex with
	| Bool true -> true
	| Bool false -> false
	| Not x -> not(eval x)
	| And (a, b) -> eval(a) && eval(b)
	| Var x -> failwith("Are variabile, esti prost");;

(*Să se scrie o funcție evalueaza care ia ca argument o listă de 
  perechi nume-valoare de adevăr și o expresie logică 
  cu variabile și întoarce valoarea ei de adevăr.
 *)

let rec cauta l x = match l with
	| [] -> failwith "Nu exista"
	| a :: b -> if x = fst(a) then snd(a)
	            else cauta b x;;


let rec evalueaza l ex = match ex with
	| Bool true -> true
	| Bool false -> false
	| Not a -> not(evalueaza l a)
	| And (a, b) -> (evalueaza l a) && (evalueaza l b)
	| Var x -> cauta l x;;

let rec simplifica ex = match ex with
	| Bool true -> Bool true
	| Bool false -> Bool false
	| Not a -> not(simplifica a)
	| And (a, b) -> (simplifica a) && (simplifica b)
	| Var x -> Var x;;
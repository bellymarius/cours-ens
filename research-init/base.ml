let to_string l =  List.fold_left (fun s x ->  s ^ (Printf.sprintf " %d " x) ) "" l


let log b n =
  let rec aux m k = if m<b then k else aux (m/b) (k+1) in
  aux n 0;;



let pow b k = 
let rec aux a i = if i=0 then a else aux (b*a) (i-1) in
aux 1 k;;

let convert_from l b = (* convert to base 10*)
	let rec aux l' m a i = match l' with
		|[] -> m
		|c::r -> aux r (m+c*a) (a*b) (i+1)
in
aux (List.rev l) 0 1 0;;


let convert_to n b = (* convert from base 10*)
	let i0 = log b n in
	let a0 = pow b i0 in
	let rec aux l m a i = if i<0 then l (* m = cb^i + r = ca + r  *)
		else let c = m / a in 
			 let m' = m - c*a in
			 let a' = a/b in
			 aux (c::l) m' a' (i-1)
	in
	List.rev (aux [] n a0 i0);;


let sequence s m = (*Compute the m first terms of the sequence using seed s*)
	let rec aux l t i = if i = m || t<0 then List.rev l
		else let u = convert_to t (i+2) in
			 let v = convert_from u (i+3) in
			 aux (t::l) (v-1) (i+1);
	in
	aux [] s 0;;


Printf.printf "log 2 0 = %d [Expected 0]\n" (log 2 0);;
Printf.printf "log 2 1 = %d [Expected 0]\n" (log 2 1);;
Printf.printf "log 2 2 = %d [Expected 1]\n" (log 2 2);;
Printf.printf "pow 2 10 = %d [Expected 1024]\n" (pow 2 10);;
Printf.printf "pow 3 3 = %d [Expected 27]\n" (pow 3 3);;
Printf.printf "convert_to 10 2 = %s [Expected 1010]\n" (to_string (convert_to 10 2));;
Printf.printf "convert_from 1010 2 = %d [Expected 10]\n" (convert_from [1;0;1;0] 2);;
Printf.printf "sequence 2 5 = %s [Expected 2210]\n" (to_string (sequence 2 5));;
Printf.printf "sequence 100 10000 = %s \n" (to_string (sequence 100 10000));;


(* Proof of termination : exists b such that a0...ak < b0...bk implies (a0...ak) <lex (b0...bk) implies a0...ak < b0...bk in base w *)

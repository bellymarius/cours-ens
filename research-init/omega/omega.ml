let rand () = 5 + Random.int 7;;

let pretty_print t = Printf.printf "[ "; Array.iter (fun x -> Printf.printf "%d " x) t; Printf.printf "]\n";;

let prog () = 
let n=rand () in
let i=ref 0 in
let ord = Array.init n (fun m -> rand ()) in
pretty_print ord;
while not (Array.for_all (fun x -> x<0) ord) do
	if ord.(!i) > 0 then 
	begin
	ord.(!i)<-ord.(!i)-1;
	if !i>0 then ord.(!i -1)<-ord.(!i -1)+rand();
	pretty_print ord;
	i := 0
	end
	else i := !i+1
done;;

prog()


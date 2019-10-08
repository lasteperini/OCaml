(* 1. Write a function last : 'a list -> 'a option that returns the last element of a list. (easy)*)


let rec last = function
    | [] -> None
    | [x] -> Some x
    | _ :: t -> last t;;  

let get_string data = 
  match data with
   None -> ""
   | Some str -> str 

let get_int data = 
  match data with
   None -> 0
   | Some int -> int 

let main() =
	print_endline("last([\"a\";\"b\";\"c\";\"d\"]) :- "^get_string(last(["a";"b";"c";"d"])));	
	print_endline("last([]) :- "^get_string(last([])));
	print_endline("last([1;4;5]) :- "^string_of_int(get_int(last([1;4;5]))));;	

main();;

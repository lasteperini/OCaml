(*Write the matrix datatype with the following operations:

A function zeroes to construct a matrix of size n×m filled with zeros.
A function identity to construct the identity matrix (the one with all 0s but the 1s on the diagonal) of given size.
A function init to construct a square matrix of a given size n filled with the first n×n integers.
A function transpose that transposes a generic matrix independently of its size and content.
A function * that multiplies two matrices non necessarily square matrices.*)

let rec buildstringrow = function
	|[] -> ""
	|x::tl ->  " " ^ string_of_int(x) ^ buildstringrow(tl)

let rec buildstring = function
	|[] -> ""
	|x::tl ->  "\n" ^ buildstringrow(x) ^ buildstring(tl)

let rec zeros n m = 
	let rec buildMatrixCol acc m = 
	match m with
	|0 -> List.rev acc
	|_ -> buildMatrixCol (0::acc) (m-1) in
	let rec buildMatrix acc n m = 
	match n with
	|0 -> List.rev acc
	|_ -> buildMatrix ((buildMatrixCol [] m)::acc) (n-1) m in
	buildMatrix [] n m;;

print_endline("zeros 3 5:\n" ^ buildstring(zeros 3 5) ^ "\n\n");;
		
let rec identity n = 
	let rec buildMatrixCol acc n m = 
	match m with
	|0 -> List.rev acc
	|_ -> if n = m then buildMatrixCol (1::acc) n (m-1) else buildMatrixCol (0::acc) n (m-1) in
	let rec buildMatrix acc n m = 
	match n with
	|0 -> List.rev acc
	|_ -> buildMatrix ((buildMatrixCol [] n m)::acc) (n-1) m in
	buildMatrix [] n n;;

print_endline("identity 4:\n" ^ buildstring(identity 4) ^ "\n\n");;

(*changed from1 to 2 args*)
let rec init n m = 
	let rec buildMatrixCol acc counter n m = 
	match m with
	|0 -> List.rev acc
	|_ -> buildMatrixCol(counter::acc) (counter+1) n (m-1) in
	let rec buildMatrix acc counter n m = 
	match n with
	|0 -> List.rev acc
	|_ -> buildMatrix ((buildMatrixCol [] (counter) n m)::acc) (counter+m) (n-1) m in
	buildMatrix [] 0 n m;;


print_endline("init 2 4:\n" ^ buildstring(init 2 4) ^ "\n\n");;

let rec transpose matrix =
	let rec getValue elem n  = function
	|[] -> raise Not_found
	|x::tl -> if elem = n then x else getValue elem (n+1) tl in
	let rec recordCol acc counter = function
	|[] -> List.rev acc
	|x::tl -> recordCol ((getValue counter 0 x )::acc) counter tl in
	let rec parseFirstRow acc length count matrix = 
	if length = count then List.rev acc else parseFirstRow ((recordCol [] count matrix)::acc) length (count+1) matrix in
	let readFirst acc = function
	|[] -> raise Not_found
	|x::tl ->  parseFirstRow acc (List.length x) 0 matrix in
	readFirst [] matrix;;

print_endline("transpose(init 2 4):\n" ^ buildstring(transpose(init 2 4)) ^ "\n\n");;

let rec \* matrix1 matrix2 =
	

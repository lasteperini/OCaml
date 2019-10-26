(* Put into a list, called alkaline_earth_metals, the atomic numbers of the six alkaline earth metals: 
beryllium (4), magnesium (12), calcium (20), strontium (38), barium (56), and radium (88). Then

Write a function that returns the highest atomic number in alkaline_earth_metals.
Write a function that sorts alkaline_earth_metals in ascending order (from the lightest to the heaviest).
Put into a second list, called noble_gases, the noble gases: helium (2), neon (10), argon (18), krypton (36), xenon (54), and radon (86). 
Then write a function (or a group of functions) that merges the two lists and print the result as couples (name, atomic number) 
sorted in ascending order on the element names.
*)

let alkalines = ["beryllium",4 ;"strontium",38;  "magnesium", 12;"radium",88; "calcium", 20;"barium",56 ];;
let alkalines_nb = [56; 12; 38; 20; 4; 88];;
let nobles_gases = ["helium", 2;"xenon", 54; "argon", 18; "neon", 10; "krypton", 36; "radon", 86]
let nobles_gases_nb = [2; 10; 18; 36; 54; 86]

let getMax list = 
	let rec aux (a,max) = function
	|[] -> (a,max)
	|(n,x)::tl ->  
	if x > max then aux (n,x) tl else aux (a,max) tl in
	aux ("",0) list;;

let ascending list =
	let rec aux acc = function
	|[] -> acc
	|(n,x)::tl -> 
	let (a,max) = getMax tl in
	if x > max then aux ((n,x)::acc) tl else aux acc (tl@[(n,x)]) in
	aux [] list;;

let merge_and_print list1 list2 =
	let rec buildstring = function
	|[] -> ""
	|(el,x)::tl ->  "\t" ^ el ^ "\t\t" ^ string_of_int(x) ^ "\n" ^ buildstring(tl) in
	let rec merge acc = function
	|[] -> acc
	|x::tl -> merge (x::acc) tl in 
	print_endline(buildstring (ascending (merge list1 list2)));;




let main() =
	merge_and_print alkalines nobles_gases;;
		
main();;

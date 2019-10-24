(*https://ocaml.org/learn/tutorials/99problems.html*)

(*1. Write a function last : 'a list -> 'a option that returns the last element of a list. (easy)*)

let rec last = function
	|[] -> None
	|[x] -> Some x
	|_::tl -> last tl;;
	
last [ "a" ; "b" ; "c" ; "d" ];;
last [];;


(*2. Find the last but one (last and penultimate) elements of a list. (easy)*)

let rec last_two = function
	|[] -> None
	|[x] -> None
	|[x;y] -> Some (x,y)
	|_::tl -> last2 tl;;
	
last_two [ "a" ; "b" ; "c" ; "d" ];;
last_two [ "a" ];;

(*3. Find the k'th element of a list. (easy)*)

let rec at k = function
	|[] -> None
	|x::tl -> if k = 1 then Some x else at (k-1) tl;;	

(*4. Find the number of elements of a list. (easy)*)

let length l = 
	let rec aux count = function
	|[] -> count
	|_::tl -> aux (count+1) tl
	in aux 0 l;;

(*5. Reverse a list. (easy)*)

let reverse list =
	let rec aux lout = function
	|[] -> lout
	|x::tl -> aux (x::lout) tl
	in aux [] list;;

(*6. Find out whether a list is a palindrome. (easy)*)

let is_palindrome list = 
	match list with
	|[] -> true
	|_	-> list = reverse(list);;

(*7. Flatten a nested list structure. (medium)*)
(* There is no nested list type in OCaml, so we need to define one
     first. A node of a nested list is either an element, or a list of
     nodes. *)
  type 'a node =
    | One of 'a 
    | Many of 'a node list;;
type 'a node = One of 'a | Many of 'a node list

(*7. Flatten a nested list structure. (medium)*)

let flatten list =
	let rec aux acc = function
	|[] -> acc (* fine recursione*)
	|One x::tl -> aux (x::acc) tl (*appendo il dato alla lista*)
	|Many l::tl -> aux (aux acc l) tl (*caso many: prima srotolo la lista*)
	in  reverse (aux [] list);;

(*8. Eliminate consecutive duplicates of list elements. (medium)*)

let compress list =
	let rec aux acc lastValue= function
	|[] -> acc
	|x::tl -> if x = lastValue then aux acc x tl else aux (x::acc) x tl
	in reverse (aux [] "" list);;

(*let rec compress = function
    | a :: (b :: _ as t) -> if a = b then compress t else a :: compress t
    | smaller -> smaller;;*)
	
(*9. Pack consecutive duplicates of list elements into sublists. (medium)*)

let pack list =
    let rec aux current acc = function
      | [] -> []    (* Can only be reached if original list is empty *)
      | [x] -> (x :: current) :: acc 
      | a :: (b :: _ as t) -> (*read first and second element of a list. IMPORTANT*)
         if a = b then aux (a :: current) acc t
         else aux [] ((a :: current) :: acc) t  in
    List.rev (aux [] [] list);;

(*10. Run-length encoding of a list. (easy)*)
let encode list =
    let rec aux current count = function
      | [] -> []    
      | [x] -> ((count+1,x) :: current)
      | a :: (b :: _ as t) -> 
         if a = b then aux current (count+1) t
         else aux ((count+1,a) :: current) 0 t  in
    List.rev (aux [] 0 list);;

(*11. Modified run-length encoding. (easy)*)
(*# encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
- : string rle list =
[Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
 Many (4, "e")]*)
 
type 'a rle =
    | One of 'a
    | Many of int * 'a;;

let encode l =
    let create_tuple cnt elem =
      if cnt = 1 then One elem
      else Many (cnt, elem) in
    let rec aux count acc = function
      | [] -> []
      | [x] -> (create_tuple (count+1) x) :: acc
      | hd :: (snd :: _ as tl) ->
          if hd = snd then aux (count + 1) acc tl
          else aux 0 ((create_tuple (count + 1) hd) :: acc) tl in
      List.rev (aux 0 [] l);;
 
 (*12. Decode a run-length encoded list. (medium)
 
 # decode [Many (4,"a"); One "b"; Many (2,"c"); Many (2,"a"); One "d"; Many (4,"e")];;
- : string list =
["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]*)

let decode list =
	let rec many acc n x =
	if n = 0 then acc else many (x::acc) (n-1) x in
	let rec aux acc = function
	|[] -> acc
	|One x::t -> aux (x::acc) t
	|Many (n,x)::t -> aux (many acc n x) t in
	List.rev (aux [] list);; 

(*13. Run-length encoding of a list (direct solution). (medium)
# encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
- : string rle list =
[Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
 Many (4, "e")]*)
 
let encode list =
	let many acc n x = 
	if n = 1 then (One x)::acc else (Many (n,x)):: acc in
	let rec aux count acc = function
	|[] -> []
	|[x] -> (many acc (count+1) x)
	|a::(b::_ as t) ->
	if a = b then aux (count+1) acc t
	else aux 0 (many acc (count+1) a) t in
	List.rev (aux 0 [] list);;

(*14. Duplicate the elements of a list. (easy)*)

let duplicate list = 
	let rec aux acc = function
	|[] -> acc
	|x::t -> aux (x::(x::acc)) t in
	List.rev (aux [] list);;

(*15. Replicate the elements of a list a given number of times. (medium)*)

let replicate list n =
	let rec rep acc elem k = 
	if k = 0 then acc else rep (elem::acc) elem (k-1) in
	let rec aux acc n = function
	|[] -> acc
	|x::t -> aux (rep acc x n) n t in
	List.rev (aux [] n list) ;;
	
(*16. Drop every N'th element from a list. (medium)*)

let drop list elem =
	let divide sup inf count =
		sup/inf = count in
	let rec aux acc sup inf count = function
	|[] -> acc 
	|x::t -> 
	if (divide sup inf count) then aux acc (sup+1) inf (count+1) t 
	else aux (x::acc) (sup+1) inf count t in
	List.rev (aux [] 1 elem 1 list);;

(*17. Split a list into two parts; the length of the first part is given. (easy)*)

let split list elem = 
	let rec aux acc count elem = function
	|[] -> [List.rev acc;[]] 
	|x::t -> 
	if (count=elem) then [List.rev (x::acc); t]
	else aux (x::acc) (count+1) elem t in
	aux [] 1 elem list;;

(*18. Extract a slice from a list. (medium)*)

let slice list n k = 
	let rec aux acc count n k = function
	|[] -> List.rev acc
	|x::t -> 
	if count > (n-1) && count < (k+1) then aux (x::acc) (count+1) n k t
	else aux acc (count+1) n k t in
	aux [] 0 n k list;;

(*19. Rotate a list N places to the left. (medium)*)

let rotate list n =
	let invert l elem =
		if elem < 0 then (List.length l) + elem - 1 else elem in
	let rec aux acc count n = function
	|[] -> List.rev acc
	|x::t ->
	if count < n then aux (x::acc) (count+1) n t
	else t @ (List.rev acc) in
	aux [] 0 (invert list n) list;;
	
(*20. Remove the K'th element from a list. (easy)*)

let remove_at n list =
	let rec aux acc count n = function
	|[] -> List.rev acc
	|x::t ->
	if count <> n then aux (x::acc) (count+1) n t
	else aux acc (count+1) n t in
	aux [] 0 n list;;
	


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
	
(*21. Insert an element at a given position into a list. (easy)*)

let insert_at elem n list =
	let rec aux acc count n elem = function
	|[] -> List.rev (if count < (n+1) then  (elem::acc) else acc) 
	|x::t ->
	if count = n then aux (x::elem::acc) (count+1) n elem t
	else aux (x::acc) (count+1) n elem t in
	aux [] 0 n elem list;;

(*22. Create a list containing all integers within a given range. (easy)*)

let range a b = 
	let rec aux acc a b  = 
		if a < b then aux (a::acc) (a+1) b else List.rev (b::acc) in
	if (a < b ) then aux [] a b else List.rev (aux [] b a) ;;

(*23. Extract a given number of randomly selected elements from a list. (medium)*)
(*official solution*)
let rand_select list n =
	(*extract [] 4 [2;5;8];; 
	Exception: Not_found.
	extract [] (Random.int 3) [2;5;8];;
- : int * int list = (8, [5; 2])
	*)
	
    let rec extract acc n = function
      | [] -> raise Not_found (*exception*)
      | h :: t -> if n = 0 then (h, acc @ t) else extract (h::acc) (n-1) t
    in
    let extract_rand list len =
      extract [] (Random.int len) list
    in
    let rec aux n acc list len =
      if n = 0 then acc else
        let picked, rest = extract_rand list len in
        aux (n-1) (picked :: acc) rest (len-1)
    in
    let len = List.length list in
    aux (min n len) [] list len;;
    
 (*Arithmetic*)
(*31. Determine whether a given integer number is prime. (medium)*)

let is_prime n =
	(*from solution: *)
	let n = abs n in
	(**)
	let rec aux m n = 
	if m > n/2 then true 
	else if (n mod m) = 0 then false else aux (m+1) n in
	aux 2 n;;

(*32. Determine the greatest common divisor of two positive integer numbers. (medium)*)

let rec gcd a b =
	if b = 0 then a else gcd b (a mod b);;

(*33. Determine whether two positive integer numbers are coprime. (easy)*)

let coprime a b =
	(gcd a b) = 1 ;;

(*34. Calculate Euler's totient function φ(m). (medium)
Euler's so-called totient function φ(m) is defined as the number of positive integers 
r (1 ≤ r < m) that are coprime to m. We let φ(1) = 1.

Find out what the value of φ(m) is if m is a prime number. Euler's totient function 
plays an important role in one of the most widely used public key cryptography methods 
(RSA). In this exercise you should use the most primitive method to calculate this function 
(there are smarter ways that we shall discuss later).*)

let phi m = 
	let rec aux count n m = 
	if n = 1 then (count+1) 
	else if (coprime n m) then aux (count+1) (n-1) m else aux count (n-1) m in
	aux 0 m m ;; 

(*35. Determine the prime factors of a given positive integer. (medium)*)

let factors n =
	let rec aux acc m n =
	if n = 1 then [] else
	if m > n then List.rev acc
	else if (n mod m) = 0 then aux (m::acc) 2 (n/m) else aux acc (m+1) n in
	aux [] 2 n;;

(*37. Calculate Euler's totient function φ(m) (improved). (medium)*)

(* from solutions *)
let rec pow n p = if p < 1 then 1 else n * pow n (p-1);;
(* [factors] is defined in the previous question. *)
let phi_improved n =
	let rec aux acc = function
	  | [] -> acc
	  | (p,m) :: t -> aux ((p - 1) * pow p (m - 1) * acc) t in
	aux 1 (factors n);;
	
(*39. A list of prime numbers. (easy)*)

let all_primes a b =
	let rec aux acc = function
	|[] -> List.rev acc
	|x::t -> if is_prime x then x::(aux acc t) else aux acc t in
	aux [] (range a b);; 

(*40. Goldbach's conjecture. (medium)*)

let goldbach n =
	let rec sum_all a n = function
	|[] -> (0,0)
	|x::t -> if (a + x) = n then (a,x) else (sum_all a n t) in
	let rec aux n = function
	|[] -> raise Not_found
	|x::t -> 
	let res = sum_all x n t in
	if res = (0,0) then aux n t else res in	
	if n = 1 || n = 2 then raise Not_found
	else aux n (all_primes 3 n);;
	

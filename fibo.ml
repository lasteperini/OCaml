open List;;
let rec fibo(n) = if n<=1 then n else fibo(n-1) + fibo(n-2);;
let main() =
	let ins = [5; 7; 15; 25; 30] in
	for i=0 to List.length ins -1 do
		print_endline("fibo("^string_of_int(nth ins i)^") :- "^string_of_int(fibo(nth ins i)));
	done;;
main();;

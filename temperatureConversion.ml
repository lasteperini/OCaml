(*Beyond the well-known Celsius and Fahrenheit, there are other six temperature scales: Kelvin, Rankine, Delisle, Newton, Réaumur, and Rømer 
(Look at http://en.wikipedia.org/wiki/Comparison_of_temperature_scales to read about them).

Write a function that given a pure number prints a conversion table for it among any of the 8 scales (remember that functions are high-order).
Write a function that given a temperature in a specified scale returns a list of all the corresponding temperatures in the other scales, note 
that the scale must be specified (hint: use a tuple).*)


let convert n = 
	let scales = ["celsius";"fahrenheit";"kelvin";"rankine";"deslise";"newton";"reaumur";"romer" ] in
	let rec fromCelsius dest n=
	match dest with
	|"fahrenheit" -> ((n *. 9.0) /. 5.0) +. 32.0
	|"kelvin" -> n +. 272.15
	|"rankine" -> (fromCelsius "kelvin" n) *. (9.0 /. 5.0)
	|"deslise" -> (100.0 -. n) *. 1.5
	|"newton" -> n *. (33.0 /. 100.0)
	|"reaumur" -> n *. 0.8
	|"romer" -> (n *. (21.0 /. 40.0)) +. 7.5
	|_ -> n in
	let rec toCelsius dest n =
	match dest with
	|"fahrenheit" -> (n -. 32.0) *. (5.0 /. 9.0)
	|"kelvin" ->  n -. 273.15
	|"rankine" -> (toCelsius "kelvin" (toCelsius "kelvin" n)) *. (5.0 /. 9.0)
	|"deslise" -> 100.0 -. n *. (2.0 /. 3.0)
	|"newton" -> n *. (100.0 /. 33.0)
	|"reaumur" ->  n *. 1.25
	|"romer" -> (n -. 7.5) *. (40.0 /. 21.0) 
	|_ -> n in
	let rec crossTableCalcLine acc n = function
	|[] -> List.rev acc
	|(source,dest)::tl -> crossTableCalcLine (((source, dest, fromCelsius dest (toCelsius source n)))::acc) n tl in
	let rec crossTableCalc acc n = function
	|[] -> List.rev acc
	|x::tl -> crossTableCalc ((crossTableCalcLine [] n x)::acc) n tl in
	let rec createLineCrossTable acc elem = function
	|[] -> List.rev acc
	|x::tl -> createLineCrossTable ((x,elem)::acc) elem tl in
	let rec createCrossTable acc = function
	|[] -> List.rev acc
	|x::tl ->  createCrossTable ((createLineCrossTable [] x scales)::acc) tl in	
	crossTableCalc [] n (createCrossTable [] scales);;
	

let inmulteste a b = 
	let x = ref a
	and y = ref b
	and z = ref 0
	in z := 0 ;
		while 1 <= !x do
			z := !z + !y;
			x := !x + -1
		done
		; [("x", !x);("y", !y);("z", !z)]
;;
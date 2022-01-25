
(* Vrne naslednika števila n. *)
fun next (n : int) = n + 1

(* Vrne vsoto števil a in b. *)
fun add (a : int, b : int) = a + b

(* Vrne true, če sta vsaj dva argumenta true, drugače vrne false *)
fun majority (a : bool, b : bool, c : bool) = a andalso b orelse a andalso c orelse b andalso c

(* Vrne mediano argumentov - števila tipa real brez (inf : real), (~inf : real), (nan : real) in (~0.0 : real)
   namig: uporabi Real.max in Real.min *)
fun median (a : real, b : real, c : real) = 
   if (a > b) then
      if (a < c) then a
      else if (b > c) then b
      else c
   else
      if (a > c) then a
      else if (b < c) then b
      else c

(* Preveri ali so argumenti veljavne dolžine stranic nekega trikotnika - trikotnik ni izrojen *)
fun triangle (a : int, b : int, c : int) = (a + b > c) andalso (a + c > b) andalso (b + c > a)

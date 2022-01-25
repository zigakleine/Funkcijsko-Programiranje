(*  Vrne fakulteto števila n, n >= 0. *)
fun factorial (n : int) = 
    case n <= 1 of
    true => 1
    | false => n * factorial (n-1)

(*  Vrne n-to potenco števila x, n >= 0. *)
fun power (x : int, n : int) =
	case n of
	0 => 1 
	| _ => x * power(x, n-1)

(*  Vrne največjega skupnega delitelja pozitivnih števil a in b, a >= b. *)
fun gcd (a : int, b : int) =
    case b of
    0 => a
    | _ => gcd(b, a mod b)

(*  Vrne dolžino seznama. *)
fun len (xs : int list) =
    case xs of
    [] => 0
    | _::tl => 1 + len tl

(*  Vrne SOME zadnji element seznama. Če je seznam prazen vrne NONE. *)
fun last (xs : int list) = 
    case xs of
    [] => NONE
    | hd :: [] => SOME hd
    | hd :: tl => last(tl)

(*  Vrne SOME n-ti element seznama. Prvi element ima indeks 0. Če indeks ni veljaven, vrne NONE. *)
fun nth (xs : int list, n : int) = 
    case (xs, n) of
    ([], _) => NONE
    | (hd::_, 0) => SOME hd
    | (hd::tl, x)  => nth(tl, n-1)


(*  Vrne nov seznam, ki je tak kot vhodni, le da je na n-to mesto vrinjen element x. Prvo mesto v seznamu ima indeks 0. Indeks n je veljaven (0 <= n <= length xs). *)
fun insert (xs : int list, n : int, x : int) =
    case (xs, n) of 
    (_, 0) => x::xs
    | (hd::tl, _) => hd::insert(tl, n-1, x)

(*  Vrne nov seznam, ki je tak kot vhodni, le da so vse pojavitve elementa x odstranjene. *)
fun delete (xs : int list, x : int) =
    case (xs, null xs orelse hd xs = x) of
    ([], _) => []
    | (hd::tl, true) => delete(tl, x)
    | (hd::tl, false) => hd::delete(tl, x)  

(*  Vrne obrnjen seznam. V pomoč si lahko spišete še funkcijo append, ki doda na konec seznama. *)
fun append (xs: int list, x: int) = 
    case xs of
    [] => x::[]
    | hd::[] => hd::x::[] 
    | hd::tl => hd::append(tl,x)

fun reverse (xs : int list) =
    case xs of
    [] => []
    | hd::tl => append(reverse tl, hd)


(*  Vrne true, če je podani seznam palindrom. Tudi prazen seznam je palindrom. *)
fun palindrome (xs : int list) = xs = reverse xs

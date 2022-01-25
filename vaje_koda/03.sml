
fun foldl (_, z, []) = z
| foldl (f, z, h :: t) = foldl (f, f (z, h), t)

(* val fold = fn : ('a * 'b -> 'a) * 'a * 'b list -> 'a *)
fun fold (f, z, s) = foldl (f, z, s)

fun foldr (f, z, s) = foldl (f, z, foldl (fn (z, x) => x :: z, [], s))

(* val map = fn : ('a -> 'b) * 'a list -> 'b list *)
fun map (f, s) = foldr (fn (z, x) => f x :: z, [], s)

(* val filter = fn : ('a -> bool) * 'a list -> 'a list *)
fun filter (f, s) = 
    foldr (fn (z, x) => if f x then x :: z else z, [], s)

(* val zip = fn : 'a list * 'b list -> ('a * 'b) list *)
fun zip (xh :: xt, yh :: yt) =
    (xh, yh) :: zip (xt, yt)
| zip ([], _ ) = []
| zip (_, []) = []

(* val unzip = fn : ('a * 'b) list -> 'a list * 'b list *)
fun unzip a = 
    (map ((fn (xh, _) => xh), a), map ((fn (_, yh) => yh), a))


datatype natural = Succ of natural | One;
exception NotNaturalNumber;

datatype 'a bstree = br of 'a bstree * 'a * 'a bstree | lf;
datatype direction = L | R;

(* val subtract = fn : natural * natural -> natural *)
fun subtract (Succ a, Succ b) = 
    subtract (a, b)
| subtract (Succ a, One) = a
| subtract (One, _) = raise NotNaturalNumber

(* val any = fn : ('a -> bool) * 'a list -> bool *)
fun any (f, hd::tl) = 
    (case (f hd) of
    true => true
    | false => any(f, tl))
| any (f, []) = false

(* val rotate = fn : 'a bstree * direction -> 'a bstree *)
fun rotate (br (a, p, br (b, q, c)), L) = br (br (a, p, b), q, c)
| rotate (br (br (a, p, b), q, c), R) = br (a, p, br (b, q, c))
| rotate (br (l, i, r), _) = br (l, i, r)
| rotate (lf, _) = lf

fun height (br (l, i, r)) = 1 + Int.max(height l , height r)
| height(lf) = 1

fun balanceFactor (br (l, i, r)) = 
    height(l) - height(r)
| balanceFactor lf = 0 

(* val rebalance = fn : 'a bstree -> 'a bstree *)
fun rebalance lf = lf
| rebalance (br (l, i, r)) =
    case (balanceFactor(br(l, i, r)), balanceFactor(l), balanceFactor(r)) of
    (2,1,_) => rotate (br (l, i, r), R)
    | (2,~1,_) => rotate (br (rotate (l, L), i, r), R)
    | (~2,_,1) => rotate (br (l, i, rotate (r, R)), L)
    | (~2,_,~1) => rotate (br (l, i, r), L)
    | (_,_,_) => br (l, i, r)

(* val avl = fn : ('a * 'a -> order) * 'a bstree * 'a -> 'a bstree  *)
fun avl (_, lf, e) = br (lf, e, lf)
| avl (c, br (l, i, r), e) = 
    case c (e, i) of
    LESS => rebalance (br (avl (c, l, e), i, r))
    | GREATER => rebalance (br (l, i, avl (c, r, e)))
    | EQUAL => br (l, i, r)
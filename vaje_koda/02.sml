datatype number = Zero | Succ of number | Pred of number;

fun simp Zero = Zero
|   simp (Pred n) = 
    (case simp n of
        Succ n => n
        | n => Pred n)
|   simp (Succ n) = 
    (case simp n of
        Pred n => n
        | n => Succ n)

(*  1 -->  (Succ Zero)  *)
(*  -1 -->  (Pred Zero)  *)

(*  2 -->  Succ (Succ Zero)  *)
(*  -2 -->  Pred (Pred Zero)  *)

(*  3 -->  Succ (Succ (Succ Zero))  *)
(*  -3 -->  Pred (Pred (Pred Zero))  *)

(*  2 -->  Pred (Succ (Succ (Succ Zero)))  *)
(*  -2 -->  Succ (Pred (Pred (Pred Zero)))  *)

(* Negira število a. Pretvorba v int ni dovoljena! *)
fun neg (Zero) = Zero
|   neg (Pred n) = Succ (neg n)
|   neg (Succ n) = Pred (neg n)

(* Vrne vsoto števil a in b. Pretvorba v int ni dovoljena! *)
fun add (Zero, Zero) = Zero
|   add ((x, Zero) | (Zero, x)) = x
|   add (a, Succ b) = add (Succ a, b)
|   add (a, Pred b) = add (Pred a, b)

(* Vrne rezultat primerjave števil a in b. Pretvorba v int ter uporaba funkcij `add` in `neg` ni dovoljena!
    namig: uporabi funkcijo simp *)
fun comp (a : number, b : number) =
    case (simp a, simp b) of
    (Zero, Zero) => EQUAL
    | ((Zero, Pred _) | (Succ _, Zero) | (Succ _, Pred _)) => GREATER
    | ((Zero, Succ _) | (Pred _, Zero) | (Pred _, Succ _)) => LESS
    | ((Succ a, Succ b) | (Pred a, Pred b)) => comp(a, b)



datatype tree = Node of int * tree * tree | Leaf of int;

(* Vrne true, če drevo vsebuje element x. *)
fun contains(Leaf i, x) = (i = x)
|   contains (Node (i, l, r), x) = (i = x) orelse contains (l, x) orelse contains (r, x)

(* Vrne število listov v drevesu. *)
fun countLeaves (Node (_, l, r)) = countLeaves l  + countLeaves r
| countLeaves(Leaf _) = 1 

(* Vrne število število vej v drevesu. *)
fun countBranches (Node (_, l, r)) = 2 + countBranches l + countBranches r
| countBranches(Leaf _) = 0

(* Vrne višino drevesa. Višina lista je 1. *)
fun height (Node (i, l, r)) = 1 + Int.max(height l , height r)
| height(Leaf _) = 1

(* Pretvori drevo v seznam z vmesnim prehodom (in-order traversal). *)
fun toList(Node (i, l, r)) = toList l @ [i] @ toList r
| toList (Leaf i) = [i]

(* Vrne true, če je drevo uravnoteženo:
 * - Obe poddrevesi sta uravnoteženi.
 * - Višini poddreves se razlikujeta kvečjemu za 1.
 * - Listi so uravnoteženi po definiciji.
 *)
fun isBalanced (Node (i, l, r)) = 
    isBalanced(l) = (abs(height(l) - height(r)) <= 1) andalso isBalanced l  andalso isBalanced r
| isBalanced(Leaf i) = true

(* Vrne true, če je drevo binarno iskalno drevo:
 * - Vrednosti levega poddrevesa so strogo manjši od vrednosti vozlišča.
 * - Vrednosti desnega poddrevesa so strogo večji od vrednosti vozlišča.
 * - Obe poddrevesi sta binarni iskalni drevesi.
 * - Listi so binarna iskalna drevesa po definiciji.
 *)
fun treeElementValue(Node (i, l, r)) = i
| treeElementValue(Leaf i) = i

fun isBST (Node (i, l, r)) = treeElementValue l  < i andalso i < treeElementValue r andalso isBST l andalso isBST r
| isBST(Leaf i) = true
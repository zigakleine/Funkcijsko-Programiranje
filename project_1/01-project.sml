val _ = Control.Print.printDepth := 10;
val _ = Control.Print.printLength := 10;
val _ = Control.Print.stringDepth := 2000;
val _ = Control.polyEqWarn := false;


fun readFile filename =
  let val is = TextIO.openIn filename
  in 
    String.map (fn c => if Char.isGraph c orelse c = #" " orelse c = #"\n" then c else #" ")
      (TextIO.inputAll is)
    before TextIO.closeIn is
  end

exception NotImplemented;

(* int -> 'a list -> 'a list list *)
(* fun split blockSize sez *)
fun split blockSize sez = 
    let
        fun splitAt (n, acc, sez) = 
            case (n>0, sez) of
            (true, []) => NONE
            | (false, []) => SOME (List.rev acc, [])
            | (true, x::xs) => splitAt (n-1, x::acc, xs)
            | (false, x::xs) => SOME (List.rev acc, sez)
    in
        case splitAt (blockSize, [], sez) of
        NONE => []
        | SOME (block, rest) => block::(split blockSize rest)
    end

signature RING =
sig
  eqtype t
  val zero : t
  val one : t
  val neg : t -> t
  val inv : t -> t option
  val + : t * t -> t
  val * : t * t -> t
end;

functor Ring (val n : int) :> RING where type t = int =
struct
  type t = int
  val zero = 0
  val one = 1
  fun neg x = ~x mod n

  (* int * int -> int option *)
  fun modularInverse (a, n) = List.find (fn b => (b*a) mod n = one) (List.tabulate (n, fn x => x))

  fun inv x = modularInverse (x mod n, n)
  fun op + a =  Int.+ a mod n
  fun op * p =  Int.* p mod n
end;

signature MAT =
sig
  eqtype t
  structure Vec :
    sig
      val dot : t list -> t list -> t
      val add : t list -> t list -> t list
      val sub : t list -> t list -> t list
      val scale : t -> t list -> t list
    end
  val tr : t list list -> t list list
  val mul : t list list -> t list list -> t list list
  val id : int -> t list list
  val join : t list list -> t list list -> t list list 
  val inv : t list list -> t list list option
end;

functor Mat (R : RING) :> MAT where type t = R.t =
struct
  type t = R.t
  structure Vec =
    struct
      fun dot v1 v2 = List.foldl (fn (x, y) => R.+(x, y)) R.zero (ListPair.map (fn (vi1, vi2) => R.*(vi1, vi2)) (v1, v2))
      fun add v1 v2 = 
          case (v1, v2) of
          (x1::xs1, x2::xs2) => (R.+(x1, x2))::(add xs1 xs2)
          | (([],xs) | (xs,[])) => xs

      fun sub v1 v2 = 
          case (v1, v2) of
          (x1::xs1, x2::xs2) => (R.+(x1, R.neg(x2)))::(sub xs1 xs2)
          | (([],xs) | (xs,[])) => xs

      fun scale alpha v1 = List.map(fn x => R.*(alpha, x)) v1
    end

  fun tr mat =
    let
      val numRows = List.length (mat)
      val numCols = if numRows > 0
        then List.length (List.nth (mat, 0))
        else 0
    in
      List.tabulate(numCols, (fn i => List.map (fn row => (List.nth(row, i))) mat))
    end
    
  fun mul mat1 mat2 = List.foldr (fn (x, acc) => (List.map (fn a => Vec.dot x  a) (tr mat2) )::acc) [] mat1

  fun id n = 
      let 
          fun getId(i)=
              if i = n
              then []
              else List.tabulate(n, (fn x => if x=i then R.one else R.zero))::getId(i+1)
      in
          getId(0)
      end

  fun join mat1 mat2 = 
    case (mat1, mat2) of
       (([], mat) | (mat, [])) => mat
       | (_, _) => ListPair.map (fn (rowM1, rowM2) => List.foldr(fn (x, acc) => x::acc) rowM2 rowM1) (mat1, mat2)


  fun reduce vec mat = List.map (fn row => case row of (x::xs) => Vec.sub xs (Vec.scale x vec) | [] => []) mat

  fun pivot ((x::xs)::rest) = 
    (case (R.inv x) of 
      SOME xInv =>  SOME ((Vec.scale xInv (x::xs))::rest)
      | NONE => case (pivot rest) of 
                  SOME (x2 :: rest2) => SOME (x2::(x::xs)::rest2)
                  | NONE => NONE)
  | pivot _ = NONE

  fun gauss(above, []) = SOME above
  | gauss (above, current) = 
    case pivot current of
      SOME ((one :: v') :: m) => gauss (((reduce v' above) @ [v']), (reduce v' m))
      | NONE => NONE

  fun inv mat = gauss([], (join (mat) (id (if (List.length mat > 0) then List.length (List.nth (mat, 0)) else 0))))
end;

structure RationalFiled :> RING where type t = IntInf.int * IntInf.int =
struct
  (* (a, b) <=> a/b *)
  type t = IntInf.int * IntInf.int
  open IntInf
  val zero = (0, 1) : t
  val one = (1, 1) : t
  fun neg (a, b) = (~a, b)
  fun inv ((0, _) : t) = NONE
    | inv (a, b) = SOME (b, a)
  fun gcd (a, 0) = a
    | gcd (a, b) = gcd (b, a mod b)
  fun rat (a, b) = 
    let val d = gcd (a, b)
    in (a div d, b div d) end

  fun (a, b) + (c, d) = let open IntInf in rat (a * d + c * b, b * d) end
  fun (a, b) * (c, d) = let open IntInf in rat (a * c, b * d) end
end

functor IntegerModRingMat (val n : int) :> MAT where type t = int =
struct
  structure R = Ring (val n = n)
  structure M = Mat (R)
  structure QM = Mat (RationalFiled)

  open M

  val n = IntInf.fromInt n
  fun bmod x = IntInf.toInt (IntInf.mod (x, n))

  fun inv m =
    Option.map
      (fn m =>
        map (map (fn (a, b) => R.* (bmod a, valOf (R.inv (bmod b))))) m)
      (QM.inv (map (map (fn x => (IntInf.fromInt x, 1))) m))
    handle Option => NONE
end

signature CIPHER =
sig
  type t
  val encrypt : t list list -> t list -> t list
  val decrypt : t list list -> t list -> t list option
  val knownPlaintextAttack : int -> t list -> t list -> t list list option
end;

functor HillCipherAnalyzer (M : MAT) :> CIPHER
  where type t = M.t
=
struct
  type t = M.t
  
  fun encrypt key plaintext = List.concat (List.foldr (fn (block, acc) => (hd (M.mul [block] key))::acc) [] (split (List.length key) plaintext))   

  fun decrypt key ciphertext = 
    case M.inv key of
        SOME keyInv => SOME (encrypt keyInv ciphertext)
        | NONE => NONE

  fun knownPlaintextAttack keyLength plaintext ciphertext = 
    let
      val x = List.take((split keyLength plaintext), keyLength) handle Subscript => [[]]
      val y = List.take((split keyLength ciphertext), keyLength) handle Subscript => [[]]
      val invx = (M.inv x)

      fun knownPlaintextAttackT t =
        let 
          val xNew = List.take((split keyLength plaintext), t) handle Subscript => [[]]
          val yNew = List.take((split keyLength ciphertext), t) handle Subscript => [[]]

          val xtxNew = (M.mul (M.tr xNew) xNew )
          val xtyNew = (M.mul (M.tr xNew) yNew )
          val invxtxNew = (M.inv xtxNew)

        in 
          case (invxtxNew, xNew, yNew) of
          ((_, [[]],_) | (_, _, [[]])) => NONE
          | (SOME xtxinv, _, _) => SOME (M.mul xtxinv xtyNew)
          | (NONE, _, _) => if t < (keyLength) then (knownPlaintextAttackT (t+1)) else NONE
        end
    in
      case (invx, x, y) of 
        ((_, [[]],_) | (_, _, [[]])) => NONE
        |(SOME xinv, _, _) => SOME (M.mul xinv y)
        | (NONE, _, _) => knownPlaintextAttackT 1

    end

end;


structure Trie :> 
sig
eqtype ''a dict
val empty : ''a dict
val insert : ''a list -> ''a dict -> ''a dict
val lookup : ''a list -> ''a dict -> bool
end
=
struct
  datatype ''a tree = N of ''a * bool * ''a tree list
  type ''a dict = ''a tree list

  val empty = [] : ''a dict
  fun insert w dict = 
    case (w, dict) of  
      ([], dict) => dict

      | (wx::[], []) => N(wx, true, [])::[]
      | (wx::[], (dx as (N (chx, endx, nextLevelx)))::[]) => if wx = chx then (N (chx, true, nextLevelx))::[] else (N(wx, true, []))::dx::[]
      | (wx::[], (dx as (N (chx, endx, nextLevelx)))::dxs) => if wx = chx then  (N (chx, true, nextLevelx))::dxs else dx::(insert w dxs)

      | (wx::wxs, []) => N(wx, false, (insert wxs []))::[]
      | (wx::wxs, (dx as (N (chx, endx, nextLevelx)))::[]) => if wx = chx then  (N (chx, endx, (insert wxs nextLevelx)))::[] else (N(wx, false, (insert wxs [])))::dx::[]
      | (wx::wxs, (dx as (N (chx, endx, nextLevelx)))::dxs) => if wx = chx then (N (chx, endx, (insert wxs nextLevelx)))::dxs else dx::(insert w dxs)


  fun lookup w dict =
    case (w, dict) of
        ([], dict) => false
      | (_, []) => false
      | (wx::[], (dx as (N (chx, endx, nextLevelx)))::dxs) => if wx = chx then endx else (lookup w dxs)
      | (wx::wxs, (dx as (N (chx, endx, nextLevelx)))::dxs) => if wx = chx then (lookup wxs nextLevelx) else (lookup w dxs)

end;

signature HILLCIPHER =
sig
  structure Ring : RING where type t = int
  structure Matrix : MAT where type t = Ring.t
  structure Cipher : CIPHER where type t = Matrix.t
  val alphabetSize : int
  val alphabet : char list
  val encode : string -> Cipher.t list
  val decode : Cipher.t list -> string
  val encrypt : Cipher.t list list -> string -> string
  val decrypt : Cipher.t list list -> string -> string option
  val knownPlaintextAttack :
      int -> string -> string -> Cipher.t list list option
  val ciphertextOnlyAttack : int -> string -> Cipher.t list list option
end

functor HillCipher (val alphabet : string) :> HILLCIPHER =
struct

(*printable characters*)
val alphabetSize = String.size alphabet
val alphabet = String.explode alphabet

structure Ring = Ring (val n = alphabetSize)
(* structure Matrix = Mat (Ring) *)
structure Matrix = IntegerModRingMat (val n = alphabetSize)
structure Cipher = HillCipherAnalyzer (Matrix)

fun charToNum ch = 
    let
        fun charToNumTail ch [] i = raise NotImplemented
        | charToNumTail ch (x::xs) i = if ch=x then i else (charToNumTail ch xs (i+1))
    in
        charToNumTail ch alphabet 0
    end

fun encode txt = (List.map (fn ch => charToNum ch) (String.explode txt))
fun decode code = (String.implode (List.map (fn num => List.nth(alphabet, num)) code))

local
  fun parseWords filename =
    let val is = TextIO.openIn filename
      fun read_lines is =
        case TextIO.inputLine is of
          SOME line =>
            if String.size line > 1
            then String.tokens (not o Char.isAlpha) line @ read_lines is
            else read_lines is
          | NONE => []
    in List.map (String.map Char.toLower) (read_lines is) before TextIO.closeIn is end

  val dictionary = List.foldl (fn (w, d) => Trie.insert w d) Trie.empty (List.map String.explode (parseWords "hamlet.txt")) handle NotImplemented => Trie.empty
in
  fun encrypt key plaintext = decode (Cipher.encrypt key (encode plaintext))
  fun decrypt key ciphertext =
    let 
        val decrypted = (Cipher.decrypt key (encode ciphertext))
    in
        case decrypted of
            SOME plaintext => SOME (decode plaintext)
            | NONE => NONE
    end

  fun knownPlaintextAttack keyLength plaintext ciphertext = (Cipher.knownPlaintextAttack keyLength (encode plaintext) (encode ciphertext))
  fun ciphertextOnlyAttack keyLenght ciphertext = raise NotImplemented
  end
end;

use "01-project.sml";
val _ = print "\n\n========== running public tests... ==========\n";

val _ = Control.Print.printDepth := 10;
val _ = Control.Print.printLength := 10;
val _ = Control.Print.stringDepth := 2000;
val _ = Control.polyEqWarn := false;

(* usefull for making unit tests *)
fun readFile filename =
  let val is = TextIO.openIn filename
  in 
    String.map (fn c => if Char.isGraph c orelse c = #" " orelse c = #"\n" then c else #" ")
      (TextIO.inputAll is)
    before TextIO.closeIn is
  end;


(* WIP tests ! *)

val all_tests : bool list ref = ref [];


(* ==================== PART 1 ==================== *)

val _ = print "---------- split ----------\n";
val _ : int -> 'a list -> 'a list list = split;
val test1 = split 3 [1,24,12,15,23,26,24,7,3] = [[1,24,12],[15,23,26],[24,7,3]] handle NotImplemented => false;
val test2 = split 5 [1,24,12,15,23,26,24,7,3] = [[1,24,12,15,23]] handle NotImplemented => false;
val test3 = split 1 [1,24,12] = [[1],[24],[12]] handle NotImplemented => false;
val test4 = split 4 [1,24,12] = [] handle NotImplemented => false;
val test5 = split 1 ([] : int list) = [] handle NotImplemented => false;
val _ = (all_tests := !all_tests @ [test1, test2, test3, test4, test5]);

(* ==================== PART 2 ==================== *)

signature RING =
sig
  eqtype t
  val zero : t
  val one : t
  val neg : t -> t
  val inv : t -> t option
  val + : t * t -> t
  val * : t * t -> t
end

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
end

funsig RingFN (P : sig val n : int end) = RING;
functor RingTypeTest : RingFN = Ring;

structure Z96 = Ring (val n = 96);

val _ = print "---------- modularInverse ----------\n";
val _ : Z96.t -> Z96.t option = Z96.inv;
val test1 = Z96.inv 0 = NONE handle NotImplemented => false;
val test2 = Z96.inv ~1 = SOME 95 handle NotImplemented => false;
val test3 = Z96.inv 11 = SOME 35 handle NotImplemented => false;
val test4 = Z96.inv 10 = NONE handle NotImplemented => false;
val test5 = Z96.inv 31 = SOME 31 handle NotImplemented => false;
val test6 = Z96.inv 1337 = SOME 41 handle NotImplemented => false;

structure M = Mat (Ring (val n = 27));
val _ = print "---------- add ----------\n";
val _ : M.t list -> M.t list -> M.t list = M.Vec.add;
val test1 = M.Vec.add [1,2,4] [42,~33,1] = [16,23,5] handle NotImplemented => false;
val test2 = M.Vec.add [] [] = [] handle NotImplemented => false;
val _ = (all_tests := !all_tests @ [test1, test2]);

val _ = print "---------- sub ----------\n";
val _ : M.t list -> M.t list -> M.t list = M.Vec.sub;
val test1 = M.Vec.sub [1,2,4] [42,~33,1] = [13,8,3] handle NotImplemented => false;
val test2 = M.Vec.sub [] [] = [] handle NotImplemented => false;
val _ = (all_tests := !all_tests @ [test1, test2]);

val _ = print "---------- scale ----------\n";
val _ : M.t -> M.t list -> M.t list = M.Vec.scale;
val test1 = M.Vec.scale 9 [42,~33,1] = [0,0,9] handle NotImplemented => false;
val test2 = M.Vec.scale ~1 [42,~33,1] = [12,6,26] handle NotImplemented => false;
val test3 = M.Vec.scale 0 [] = [] handle NotImplemented => false;
val _ = (all_tests := !all_tests @ [test1, test2, test3]);

val _ = print "---------- dot ----------\n";
val _ : M.t list -> M.t list -> M.t = M.Vec.dot;
val test1 = M.Vec.dot [1,2,4] [42,~33,1] = 7 handle NotImplemented => false;
val test2 = M.Vec.dot [1,2,4] [1,1,1] = 7 handle NotImplemented => false;
val test3 = M.Vec.dot [] [] = 0 handle NotImplemented => false;
val _ = (all_tests := !all_tests @ [test1, test2, test3]);

val _ = print "---------- tr ----------\n";
val _ : M.t list list -> M.t list list = M.tr;
val test1 = M.tr [[1,2,3],[3,4,5]] = [[1,3],[2,4],[3,5]] handle NotImplemented => false;
val test2 = M.tr [[1,2,3],[3,4,5],[6,7,8]] = [[1,3,6],[2,4,7],[3,5,8]] handle NotImplemented => false;
val test3 = M.tr [[~1],[100]] = [[~1,100]] handle NotImplemented => false;
val _ = (all_tests := !all_tests @ [test1, test2, test3]);

val _ = print "---------- mul ----------\n";
val _ : M.t list list -> M.t list list -> M.t list list = M.mul;
val test1 = M.mul [[~1],[100]] [[2,3]] = [[25,24],[11,3]] handle NotImplemented => false;
val test2 = M.mul [[2,3]] [[~1],[100]] = [[1]] handle NotImplemented => false;
val test3 = M.mul [[3]] [[7]] = [[21]] handle NotImplemented => false;
val test4 = M.mul [[1,3,6],[2,4,7],[3,5,8]] [[1,0,0],[0,4,0],[0,5,0]] = [[1,15,0],[2,24,0],[3,6,0]] handle NotImplemented => false;
val _ = (all_tests := !all_tests @ [test1, test2, test3, test4]);

val _ = print "---------- join ----------\n";
val _ : M.t list list -> M.t list list -> M.t list list = M.join;
val test1 = M.join [[1,3,6],[2,4,7],[3,5,8]] [] = [[1,3,6],[2,4,7],[3,5,8]] handle NotImplemented => false;
val test2 = M.join [[1,3,6],[2,4,7],[3,5,8]] [[1,2],[2,7],[3,8]] = [[1,3,6,1,2],[2,4,7,2,7],[3,5,8,3,8]] handle NotImplemented => false;
val _ = (all_tests := !all_tests @ [test1, test2]);

val _ = print "---------- id ----------\n";
val _ : int -> M.t list list = M.id;
val test1 = M.id 0 = [] handle NotImplemented => false;
val test2 = M.id 1 = [[1]] handle NotImplemented => false;
val test3 = M.id 3 = [[1,0,0],[0,1,0],[0,0,1]]  handle NotImplemented => false;
val _ = (all_tests := !all_tests @ [test1, test2, test3]);

val _ = print "---------- inv ----------\n";
val _ : M.t list list -> M.t list list option = M.inv;
val test1 = M.inv [[~1]] = SOME [[26]] handle NotImplemented => false;
val test2 = M.inv [[1,3,6],[2,4,7],[3,5,8]] = NONE handle NotImplemented => false;
val test3 = M.inv [[1,3,6],[3,4,7],[3,5,8]] = NONE  handle NotImplemented => false;
val test4 = M.inv [[1,3,6],[3,3,7],[3,5,8]] = SOME [[1,24,12],[15,23,26],[24,7,3]] handle NotImplemented => false;
val test5 = M.inv [[1,3],[3,3]] = NONE handle NotImplemented => false;
val test6 = M.inv [[1,3],[3,1]] = SOME [[10,24],[24,10]] handle NotImplemented => false;
val test7 = M.inv [[1,3],[2,6]] = NONE handle NotImplemented => false;
val _ = (all_tests := !all_tests @ [test1, test2, test3, test4, test5, test6, test7]);


(* ==================== PART 3 ==================== *)

signature CIPHER =
sig
  type t
  val encrypt : t list list -> t list -> t list
  val decrypt : t list list -> t list -> t list option
  val knownPlaintextAttack : int -> t list -> t list -> t list list option
end;

structure C = HillCipherAnalyzer (IntegerModRingMat (val n = 10));
val _ = print "---------- encrypt ----------\n";
val _ : C.t list list -> C.t list -> C.t list = C.encrypt;
val test1 = C.encrypt [[1,0,0],[0,3,0],[0,0,1]] [1,2,3,4,5,6,7,8] = [1,6,3,4,5,6] handle NotImplemented => false;
val test2 = C.encrypt [[3]] [1,2,3,4,5] = [3,6,9,2,5] handle NotImplemented => false;
val test3 = C.encrypt [[3,3],[9,2]] [] = [] handle NotImplemented => false;
val test4 = C.encrypt [[3,3],[9,2]] [1,2,3] = [1,7] handle NotImplemented => false;
val _ = (all_tests := !all_tests @ [test1, test2, test3, test4]);

val _ = print "---------- decrypt ----------\n";
val _ : C.t list list -> C.t list -> C.t list option = C.decrypt;
val test1 = C.decrypt [[3,3],[9,2]] [1,7,9] = SOME [1,2] handle NotImplemented => false;
val test2 = C.decrypt [[3]] [3,6,9,2,5] = SOME [1,2,3,4,5] handle NotImplemented => false;
val test3 = C.decrypt [[6]] [3,6,9,2,5] = NONE handle NotImplemented => false;
val test4 = C.decrypt [[1,3],[3,7]] [3,6,9,2,5] = NONE handle NotImplemented => false;
val test5 = C.decrypt [[1,3],[2,6]] [] = NONE handle NotImplemented => false;
val _ = (all_tests := !all_tests @ [test1, test2, test3, test4, test5]);

val _ = print "---------- decrypt ----------\n";
val _ : int -> C.t list -> C.t list -> C.t list list option = C.knownPlaintextAttack
val test1 = C.knownPlaintextAttack 2 [1,2,3] [1,2,3] = NONE handle NotImplemented => false;
val test2 = C.knownPlaintextAttack 2 [1,2,3,5] [1,2,3,5] = SOME [[1,0],[0,1]] handle NotImplemented => false;
val test3 = C.knownPlaintextAttack 1 [1,2,3] [3,6,9] = SOME [[3]] handle NotImplemented => false;
val test4 = C.knownPlaintextAttack 1 [1,2,3] [2,4,8] = NONE handle NotImplemented => false;
val test5 = let val r = C.knownPlaintextAttack 1 [1,2,3] [5,0,5]
  in r = SOME [[5]] orelse r = NONE end (* NONE is also fine *) handle NotImplemented => false;
val test6 = C.knownPlaintextAttack 1 [2,4,5] [4,8,0] = NONE handle NotImplemented => false;
val test7 = C.knownPlaintextAttack 3 [] [] = NONE (* solution is not unique *) handle NotImplemented => false;
val test8 = C.knownPlaintextAttack 3 [1,2,3,4,5,6,7,8,9] [1,6,3,4,5,6,7,4,9] = NONE handle NotImplemented => false;
val test9 = C.knownPlaintextAttack 3 [1,2,3,4,5,6,7,8,9,1,1,1] [1,6,3,4,5,6,7,4,9,1,3,1] = NONE handle NotImplemented => false;
val test10 = C.knownPlaintextAttack 3 [1,2,3,4,5,6,7,8,9,1,1,1,1,0,0] [1,6,3,4,5,6,7,4,9,1,3,1,1,0,0] = SOME [[1,0,0],[0,3,0],[0,0,1]] handle NotImplemented => false;
val _ = (all_tests := !all_tests @ [test1, test2, test3, test4, test5, test6, test7, test8, test9, test10]);


(* ==================== PART 4 ==================== *)

(* REMOVE THE SIGNATURE AND TEST IT MANUALLY *)
(* val _ = print "---------- insert ----------\n";
val _ : ''a list -> ''a Trie.dict -> ''a Trie.dict = Trie.insert;
val t = Trie.insert (String.explode "Anamaria") Trie.empty;
val test1 = t =
  [N (#"A",false, [N (#"n",false, [N (#"a",false, [N (#"m",false, [N (#"a",false, [N (#"r",false,[N (#"i",false,[N (#"a",true,[])])])])])])])])];
val t = Trie.insert (String.explode "Ana") t;
val test2 = t =
  [N (#"A",false, [N (#"n",false, [N (#"a",true, [N (#"m",false, [N (#"a",false, [N (#"r",false,[N (#"i",false,[N (#"a",true,[])])])])])])])])]
val t = Trie.insert (String.explode "Alice") t;
val test3 = t = [N (#"A",false, [N (#"n",false, [N (#"a",true, [N (#"m",false, [N (#"a",false, [N (#"r",false,[N (#"i",false,[N (#"a",true,[])])])])])])]), N (#"l",false,[N (#"i",false,[N (#"c",false,[N (#"e",true,[])])])])])]
val t = Trie.insert (String.explode "Alan") t;
val test4 = t = [N (#"A",false, [N (#"n",false, [N (#"a",true, [N (#"m",false, [N (#"a",false, [N (#"r",false,[N (#"i",false,[N (#"a",true,[])])])])])])]), N (#"l",false, [N (#"i",false,[N (#"c",false,[N (#"e",true,[])])]), N (#"a",false,[N (#"n",true,[])])])])]
val t = Trie.insert [#"A"] t;
val test5 = t = [N (#"A",true, [N (#"n",false, [N (#"a",true, [N (#"m",false, [N (#"a",false, [N (#"r",false,[N (#"i",false,[N (#"a",true,[])])])])])])]), N (#"l",false, [N (#"i",false,[N (#"c",false,[N (#"e",true,[])])]), N (#"a",false,[N (#"n",true,[])])])])]
val _ = (all_tests := !all_tests @ [test1, test2, test3, test4, test5]);

val _ = print "---------- lookup ----------\n";
val _ : ''a list -> ''a Trie.dict -> bool = Trie.lookup;
val test1 = Trie.lookup [#"A"] t = true
val test2 = Trie.lookup (String.explode "Ana") t = true
val test3 = Trie.lookup (String.explode "Anama") t = false
val test4 = Trie.lookup (String.explode "Alan") t = true
val test5 = Trie.lookup (String.explode "Al") t = false
val test6 = Trie.lookup [] t = false
val _ = (all_tests := !all_tests @ [test1, test2, test3, test4, test5, test6]); *)

(* ==================== PART 5 ==================== *)

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

structure H = HillCipher(val alphabet = " abcdefghijklmnopqrstuvwxyz");
val _ = print "---------- encode ----------\n";
val _ : string -> H.Cipher.t list = H.encode;
val test1 = (H.encode "val a = 100;"; false) handle _ => true handle NotImplemented => false;
val test2 = H.encode "hill cipher is simple" = [8,9,12,12,0,3,9,16,8,5,18,0,9,19,0,19,9,13,16,12,5] handle NotImplemented => false;
val test3 = H.encode "" = [] handle NotImplemented => false;
val _ = (all_tests := !all_tests @ [test1, test2, test3]);

val _ = print "---------- decode ----------\n";
val _ : H.Cipher.t list -> string = H.decode;
val test1 = H.decode [] = "" handle NotImplemented => false;
val test2 = H.decode [19,0,9,0,13,0,16,0,12,0,5] = "s i m p l e" handle NotImplemented => false;
val test3 = (H.decode [101]; false) handle _ => true handle NotImplemented => false;
val _ = (all_tests := !all_tests @ [test1, test2, test3]);

val _ = print "---------- encrypt ----------\n";
val _ : H.Cipher.t list list -> string -> string = H.encrypt;
val test1 = H.encrypt [[2,3,4],[2,1,5],[14,5,11]] "hill cipher is simple" = "mltlx  bosfbbswvwurdq" handle NotImplemented => false;
val test2 = H.encrypt [[2,3,4],[2,1,5],[14,5,11]] "hill cipher is simple a" = "mltlx  bosfbbswvwurdq" handle NotImplemented => false;
val test3 = H.encrypt [[2,3],[8,22]] "hill cipher is simple but not secure" = "gflcxlkabzi hmqmnptojojimfmumfxellvb" handle NotImplemented => false;
val _ = (all_tests := !all_tests @ [test1, test2, test3]);

val _ = print "---------- decrypt ----------\n";
val _ : H.Cipher.t list list -> string -> string option = H.decrypt;
val test1 = H.decrypt [[2,3,4],[2,1,5],[14,5,11]] "mltlx  bosfbbswvwurdq" = SOME "hill cipher is simple" handle NotImplemented => false;
val test2 = H.decrypt [[2,3,4],[2,1,5],[14,5,11]] "mltlx" = SOME "hil" handle NotImplemented => false;
val test3 = H.decrypt [[6,3],[9,1]] "" = NONE handle NotImplemented => false;
val test4 = H.decrypt [[6,3],[9,1]] "xx" = NONE handle NotImplemented => false;
val test5 = H.decrypt [[2,3],[8,22]] "gflcxlkabzi hmqmnptojojimfmumfxellvb" = SOME "hill cipher is simple but not secure" handle NotImplemented => false;
val _ = (all_tests := !all_tests @ [test1, test2, test3, test4, test5]);

val _ = print "---------- knownPlaintextAttack ----------\n";
val _ : int -> string -> string -> H.Cipher.t list list option = H.knownPlaintextAttack;
val test1 = H.knownPlaintextAttack 3 "hill cipher is simple" "mltlx  bosfbbswvwurdq" = SOME [[2,3,4],[2,1,5],[14,5,11]] handle NotImplemented => false;
val test2 = H.knownPlaintextAttack 3 "hill cipher is " "mltlx  bosfbbsw" = SOME [[2,3,4],[2,1,5],[14,5,11]] handle NotImplemented => false;
val test3 = H.knownPlaintextAttack 3 "hill cipher " "mltlx  bosfb" = NONE handle NotImplemented => false;
val test4 = H.knownPlaintextAttack 2 "hill cipher" "gflcxlkabzi" = NONE handle NotImplemented => false;
val test5 = H.knownPlaintextAttack 2 "hill ciphe" "gflcxlkabz" = SOME [[2,3],[8,22]] handle NotImplemented => false;
val test6 = H.knownPlaintextAttack 2 "hill" "gflh" = NONE handle NotImplemented => false;
val _ = (all_tests := !all_tests @ [test1, test2, test3, test4, test5, test6]);

val _ = print "---------- ciphertextOnlyAttack ----------\n";
val _ : int -> string -> H.Cipher.t list list option = H.ciphertextOnlyAttack;
val test1 = H.ciphertextOnlyAttack 2 "gflcxlkabzi hmqmnptojojimfmumfxellvb" = SOME [[2,3],[8,22]] handle NotImplemented => false;
val test2 = H.ciphertextOnlyAttack 1 "xptyi smy siyy" = SOME [[5]] handle NotImplemented => false;
val test3 = H.ciphertextOnlyAttack 1 "under the tree" = SOME [[1]] handle NotImplemented => false;
val test4 = H.ciphertextOnlyAttack 2 "under the tree" = SOME [[1,0],[0,1]] handle NotImplemented => false;
val _ = (all_tests := !all_tests @ [test1, test2, test3, test4]);


val nr_passes_tests = foldl (fn (true, acc) => acc + 1 | (false, acc) => acc) 0 (!all_tests);
val nr_all_tests = length (!all_tests);

val _ = if nr_passes_tests = nr_all_tests
        then OS.Process.exit OS.Process.success
        else OS.Process.exit OS.Process.failure;
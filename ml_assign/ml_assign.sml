Control.Print.printDepth := 100;
Control.Print.printLength := 100;

(* 1. *)
fun insert x [] = [x]
|   insert x (y::ys) = if x<=y then x::(y::ys) else y::(insert x ys)
;
(* "1";
insert 13 [8,10,12,14,16]; *)

(* 2. *)
fun sort [] = []
|   sort (y::ys) = insert y (sort ys)
;
(* "2";
sort [7,3,9,2,1,10,13,6]; *)

(* 3 *)
fun polySort (op <) [] = []
|   polySort (op <) (y::ys) = 
        let fun insert (op <) x [] = [x]
            |   insert (op <) x (y::ys) = if y<x then y::(insert (op <) x ys) else x::(y::ys)
        in insert (op <) y (polySort (op <) ys)
        end
;
(* "3:";
polySort (op <) [1, 9, 3, 6, 7];
polySort (fn(a,b) => length a < length b) [[1, 9, 3, 6], [1], [2,4,6], [5,5]]; *)

(* 4 *)
fun fold f [] b = b
|   fold f (y::ys) b = f (y, (fold f ys b))
;
(* "4:";
fold (fn (x,y) => (x*x)+y) [1,2,3,4] 0; *)

(* 5 *)
datatype 'a tree = leaf of 'a | node of 'a tree list;
(* "5:";
val myTree = node [node [node [leaf [4,2,14],leaf [9,83,32],leaf [96,123,4]],
node [leaf [47,71,82]],node [leaf [19,27,10],
leaf [111,77,22,66]],
leaf [120,42,16]],
leaf [83,13]]; *)

(* 6 *)
fun fringe (leaf x) = [x]
|   fringe (node L) = fold (op @) (map fringe L) []
; 
(* "6:";
fringe (node [leaf 3, node [leaf 4, leaf 5], leaf 6]);
fringe myTree; *)

(* 7 *)
fun mapTree f (leaf x) = leaf (f x)
|   mapTree f (node L) = node (map (mapTree f) L)
;
(* "7:";
mapTree (fn x => x+100) (node [leaf 1 ,leaf 2, node [leaf 3,leaf 4]]);
mapTree (fn L => fold (op +) L 0) myTree; *)

(* 8 *)
fun sortTree (op <) T = mapTree (polySort (op <)) T;
(* "8: ";
sortTree (op <) (node [leaf [5,7,3], node [leaf [9,1,4],leaf[6,10,2]]]);
sortTree (fn (L1,L2) => length L1 < length L2)
(node [leaf [[1,2],[3],[4,5,6]],
leaf [[10,11,12], [13,14],[15]],
node [leaf [[17],[18,19,20],[21,22]]]]) ; *)

(* 9 *)
fun mergeList (op <) [] L = L
|   mergeList (op <) L [] = L
|   mergeList (op <) (y::ys) (z::zs) = 
        if y<z then y::(mergeList (op <) ys (z::zs))
        else z::(mergeList (op <) (y::ys) zs)
;
(* "9:";
mergeList (op <) [2,4,6,8] [1,3,5,7];
mergeList (op >) [8,6,4,2] [7,5,3,1]; *)

(* 10 *)
fun mergeTree (op <) T = polySort (op <) (fold (op @) (fringe T) []);
(* "10:";
mergeTree (op <) myTree; *)
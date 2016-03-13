open OUnit2
open Wildcard

let is_match x =
  x = `Match

let is_mismatch x =
  let open Wildcard in
  x = `Error Mismatch

let is_component_error x =
  let open Wildcard in
  match x with
  | `Component_error _ -> true
  | _ -> false

let tst is a b = Wildcard.match_labels a b |> is |> assert_bool (a^" = "^b)

let cmp = tst is_match

let ncmp = tst is_mismatch

let iscomperr = tst is_component_error

let tc_basic ctx =

cmp "a" "a" ;;
cmp "a" "*" ;;
cmp "a.b" "*" ;;
cmp "a.b.c" "*" ;;
cmp "a.b" "*.b" ;;
cmp "a.b.c" "*.b.c" ;;
cmp "a.b.c.d" "*.c.d" ;;

ncmp "a" "b" ;;
ncmp "a.b" "b.b" ;;
ncmp "a.b" "b.a" ;;

iscomperr "*" "" ;;
iscomperr ".b" ".b" ;;
iscomperr "a." "a." ;;
iscomperr "*" "*." ;;
iscomperr "*" ".*" ;;
iscomperr "*" "a.*" ;;

assert_bool {|test Wildcard.sort; Wildcard.sort ["a";"b";"b.c";"*.c";"a.c";"b.a.c";"*.b.a"] should return ["b.a.c"; "*.b.a"; "a.c"; "b.c"; "*.c"; "b"; "a"]|}
  (Wildcard.sort ["a";"b";"b.c";"*.c";"a.c";"b.a.c";"*.b.a"]
  = ["b.a.c"; "*.b.a"; "a.c"; "b.c"; "*.c"; "b"; "a"]) ;;


(** TODO: OUnit2 should detect test cases automatically. *)
let suite = "ts_hand" >::: [
    "tc_basic" >:: tc_basic;
  ]

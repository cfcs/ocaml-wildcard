module Wildcards : sig
  type error = A | B | C | Mismatch
  type component_error = Contains_empty_components | Asterisk_not_leftmost
  type result = [`Match | `Error of error | `Component_error of component_error]
  type component_result = [`Ok of string list | `Error of component_error]
  val components : string -> component_result
  val compare    : string -> string -> result
end =
struct
  type error = A | B | C | Mismatch
  type component_error = Contains_empty_components | Asterisk_not_leftmost
  type result = [`Match | `Error of error | `Component_error of component_error]
  type component_result = [`Ok of string list | `Error of component_error]

  let components s =
    let rec split_by_dot acc i s : string list =
      match String.index_from s i '.' with
      | next ->
         let acc  = String.(sub s i (next -i)) :: acc in
         let next = succ next in
         if next > String.length s
         then acc
         else split_by_dot acc next s
      | exception Not_found ->
         String.(sub s i (length s - i)) :: acc
    in
    let lst = split_by_dot [] 0 s in
    match lst |> List.exists (function "" -> true | _ -> false) with (* check for empty elements *)
    | true -> `Error Contains_empty_components
    | false ->
      begin match List.rev lst with (* check all elements but the leftmost for '*' *)
        | [_] | [] -> `Ok lst
        | hd::tl ->
          if tl |> List.exists (function "*" -> true | _ -> false)
          then `Error Asterisk_not_leftmost
          else `Ok lst
      end

  let compare s1 s2 =
    let rec cmp_rec = fun a b ->
    match (a : string list) , (b : string list) with
    | ""::_ , ""::_
    | []   , []
      -> `Error A
    (* 0 components are equal *)

    | [] , _
    | ""::_ , _
      -> `Error B (* 1 does not match *)

    | _ , []
    | _ , ""::_
      -> `Error C (* -1 does not match *)

    | _ , "*"::[]
    | "*"::[] , _
      ->
      `Match
      (* 0 wildcard match, asterisk must only appear as leftmost character *)

    | a_hd::[] , b_hd::[] when a_hd = b_hd
      ->
      `Match
      (*0 exactly equal, and no more elements *)

    | a_hd::a_tl , b_hd::b_tl when a_hd = b_hd
      ->
      Printf.printf "recursing: '%s' / '%s'\n" a_hd b_hd;
      cmp_rec a_tl b_tl (* compare next component *)

    | _ , _
      -> `Error Mismatch
    in
    let c1 , c2 = components s1 , components s2 in
    match c1 , c2 with
    | `Ok c1 , `Ok c2 ->
    Printf.printf ">> %d: %s\n" List.(length c1) List.(fold_left (fun a s -> "'" ^ s^ "'." ^ a) "" c1 );
    Printf.printf ">> %d: %s\n" List.(length c2) List.(fold_left (fun a s -> "'" ^ s^ "'." ^ a) "" c2) ;
    cmp_rec c1 c2
    | `Error e , _
    | _ , `Error e -> `Component_error e
    
      

end

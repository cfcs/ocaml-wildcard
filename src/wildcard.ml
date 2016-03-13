(** Wildcard matches DNS labels, respecting leftmost asterisks *)

type error = A | B | C | Mismatch
type component_error = Contains_empty_components | Asterisk_not_leftmost
type result = [`Match | `Error of error | `Component_error of component_error]
type component_result = [`Ok of string list | `Error of component_error]

type 'a labeltree =
| Labels of ((string * ('a option) * 'a labeltree) list)

(** split the string into a list of components, returning
    `Error component_error if the label is invalid *)
let components s =
  let rec split_by_dot acc i s : string list =
    match String.index_from s i '.' with
    | next ->
       let acc  = String.(lowercase @@ sub s i (next -i)) :: acc in
       let next = succ next in
       if next > String.length s
       then acc
       else split_by_dot acc next s
    | exception Not_found ->
       String.(lowercase @@ sub s i (length s - i)) :: acc
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

module Labeltree (T : sig type value end) : sig
  type t = T.value labeltree
  val empty : t
  val is_empty : t -> bool
  val asterisk : t
  val compare : t -> t -> int
  val add : t -> string -> [`Ok of t | `Component_error of component_error]
  val add_with_val : t -> T.value option -> string -> [`Ok of t | `Component_error of component_error]
  val of_string : string -> [`Ok of t | `Component_error of component_error]
  val of_list   : string list -> [`Ok of t | `Component_error of component_error]
  val of_list_with_val : (string * T.value option) list  -> [`Ok of t | `Component_error of component_error]
  val match_string : ?wildcard:bool -> t -> string -> T.value option
end = struct

type t = T.value labeltree

let empty = Labels []
let asterisk = Labels [("*", None, Labels [])]

let thrd (_, _, t) = t

let is_empty (Labels lst) = 0 = List.length lst

let put_asterisks_at_end elements ((new_name , _ , _) as v)  =
  if new_name = "*"
  then elements @ [v]
  else v :: elements

(* parses a string (domain name) into a labeltree
   adding a single value to everything *)
let add_with_val (labels : t) value s =
  let rec construct (Labels labels : 'a labeltree) lst =
    let fold_over_tree (elements, remainder) (this_name, this_value, this_subelements) =
      let this_label = (this_name, this_value, this_subelements) in
      begin match remainder with
        | Some (new_name, new_value, new_subelements)
          when new_name = this_name ->
          let merged_sublable = construct (this_subelements) new_subelements in
          put_asterisks_at_end elements (new_name, new_value, merged_sublable) , None
        | None
        | Some _ -> (put_asterisks_at_end elements this_label , remainder)
      end
    in
    begin match lst with
      | [] -> Labels labels
      | new_name :: new_sublabels ->
        begin
          match List.fold_left fold_over_tree
                  ( [], Some (new_name, value, new_sublabels)) labels
          with
          | (acc , None) -> Labels acc (* the tree was updated*)
          | (acc , Some (new_name , new_value, _ )) -> (* tree not updated; add new elements*)
            let newest = (new_name , new_value, construct (Labels []) new_sublabels) in
            Labels (put_asterisks_at_end acc newest)
        end
    end
  in
  begin match components s with
    | `Error e -> `Component_error e
    | `Ok c -> `Ok (construct labels c)
  end

let add (labels : t) s =
  add_with_val labels None s

let of_string s =
  add empty s

let of_list_with_val lst =
  List.fold_left
    (function
      | `Component_error acc -> fun _ -> `Component_error acc
      | `Ok t -> fun (s,e) ->
        begin match add_with_val t e s with
          | `Ok _ as nt -> nt
          | `Component_error _ as nc -> nc end
    ) (`Ok empty) lst

let of_list lst =
  List.map (fun e -> e , None) lst |> of_list_with_val

let compare tree1 tree2 =
  let rec checkc (Labels t1) (Labels t2) =
    begin match t1 , t2 with
      (* we have a match if there are no more components: *)
      | [] , [] -> 0

      (* reject if asymmetric empty components *)
      (* -1 is a hack since Map expects an ordered type and
         attempts to optimize lookups by sorting the keys.*)
      | [] , _ -> -1
      | _  , [] -> -1

      (* accept wildcards: *)
      | ("*" , _, Labels [])::_ , _ -> 0
      | _ , ("*" , _, Labels [])::_ -> 0

      (* shift t1 and recurse when components do not match: *)
      | (t1_name, _, _)::tl1 , (t2_name, _, _)::_
        when t1_name <> t2_name ->
        checkc (Labels tl1) (Labels t2) (* check rest of tl1 *)

      (* components match, check if any subcomponents match: *)
      | (_ , _, t1_sub)::tl1 , (_ , _, t2_sub)::_ ->
        if 0 = checkc t1_sub t2_sub
        then 0
        else checkc (Labels tl1) (Labels t2)
    end
  in checkc tree1 tree2

(** match a labeltree with a domain string, returning the associated value if it matches - asterisks in the domain string do NOT ask as wildcards (and should be illegal). *)
let match_string ?(wildcard) (Labels tree1) dns : T.value option =
  let found = ref None in (* TODO how the fuck do I do this functionally -ETIRED *)
  let rec recurse_tree lst thead : bool =
    begin match thead , lst with
      | ("*" , value, Labels []) , _::_ ->
        found := value  (* labels: wildcard match *)
      ; true
      | (th_name , value, Labels []) ,
        [lst_name] when th_name = lst_name
        ->
        found := value (* labels: exact match *)
      ; true
      | (_ , value, _) , [lst_name] when lst_name = "*" && wildcard = Some true ->
        found := value ; true (* matches the string wildcard *)
      | (th_name , _, Labels th_tl) , lst_name :: lst_tl when th_name = lst_name ->
        begin match List.find (recurse_tree lst_tl) th_tl with
          | _ -> true
          | exception Not_found -> false
        end
      | _ -> false (* apparently no match*)
    end
  in
  match components dns with
  | `Ok c ->
    begin match List.find (recurse_tree c) tree1 with
      | _ -> !found
      | exception Not_found -> None end
  | `Error _ -> None

end

(** sorts a list of labels, returning the most specific first *)
let sort labels =
  labels |> List.fold_left
    (fun a n -> match components n with
       | `Ok nls -> (n,nls)::a
       | `Error _ -> a
    ) []
  |> List.sort (fun (n_1,nls_1) (n_2,nls_2) ->
      let (len_nls_1, len_nls_2) = List.(length nls_1 , length nls_2) in
      match List.(Pervasives.compare len_nls_1 len_nls_2) with
      | 0 ->
        (* leftmost asterisk counts as less specific *)
        begin match n_1.[0] = '*' , n_2.[0] = '*' with
          | true, true | false, false -> 0
          | true, false -> 1
          | false, true -> -1
        end
      | neq -> -1 * neq
    )
  |> List.map (fun (n,_) -> n)

(** compare the two labels, returning `Match if they match (honoring wildcards),
    `Error error if they do not, and `Component_error component_error if either
    label is an invalid label *)
let match_labels s1 s2 =
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
      cmp_rec a_tl b_tl (* compare next component *)

    | _ , _
      -> `Error Mismatch
  in
  let c1 , c2 = components s1 , components s2 in
  match c1 , c2 with
  | `Ok c1 , `Ok c2 -> cmp_rec c1 c2
  | `Error e , _
  | _ , `Error e -> `Component_error e

(** returns 0 if the two labels match (honoring wildcards), -1 otherwise *)
let compare s1 s2 =
  match match_labels s1 s2 with
  | `Error _
  | `Component_error _ -> -1
  | `Match -> 0

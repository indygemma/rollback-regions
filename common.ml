(*
 * Copyright (c) 2017 Conrad Indiono
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
 * PARTICULAR PURPOSE.  See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program (see file COPYING). If not, see <http://www.gnu.org/licenses/>.
 *)
open Core

module type Node = sig(* {{{*)
  type t
  val to_string : t -> string
  val id : t -> string

  val rpst_class : t -> [`start_node | `xor_node | `and_node | `rest]
  val rpst_fragment_class : t -> [`add_fragment | `close_start_node | `close_xor_node | `close_and_node | `pub | `priv]
  val traverse_class : t -> [`maybe_successor of t option * int * int | `list_successors of t list * int * int | `stop]
  val update_node_map : t -> [`no_update | `update_single of t option | `update_multi of t list]

  val add_outgoing: t list -> t -> t
  val update_outgoing: t list -> t -> t
  val empty_start: t option
  val choose_start: t -> t option -> t option
  val is_start: t -> bool
end
(* }}}*)
(* TODO: Traversable needs a BFS traversal for unique node traversing, leave out nodes that were already visited before. At all times maintain that list *)
module type Traversable = sig(* {{{ *)
  type node_t
  val traverse  : node_t -> init   : ('a) -> f : ('a -> int -> node_t -> 'a) -> 'a
  val to_string : node_t -> string
  val bfs       : node_t -> init   : ('a) -> f : ('a -> int -> node_t -> 'a) -> 'a
end
(* }}} *)
module type NodeTransform = sig(* {{{ *)
  type source_t
  type target_t
  val transform : source_t -> target_t
end
(* }}} *)
module Participant : sig(* {{{*)
  type t
  val create : name:string -> id:Uuid.t -> t
  val name : t -> string
  val id : t -> Uuid.t
  val to_string: t -> string
end = struct
  type t = { name : string
           ; id : Uuid.t
           }
  let create ~name ~id = { name; id }
  let name self = self.name
  let id self = self.id
  let to_string self = self.name
end(* }}}*)

module StartEvent : sig(* {{{*)
  type t
  val create: id:Uuid.t -> name:string -> t
  val id: t -> Uuid.t
  val name: t -> string
  val to_string: t -> string
end = struct
  type t = { id: Uuid.t
           ; name: string
           }
  let create ~id ~name = { id; name }
  let id self = self.id
  let name self = self.name
  let to_string self = Printf.sprintf "(%s) Start:  %s"
      (Uuid.to_string self.id)
      self.name

end(* }}}*)
module EndEvent : sig(* {{{*)
  type t
  val create: id:Uuid.t -> name:string -> t
  val id: t -> Uuid.t
  val name: t -> string
  val to_string: t -> string
end = struct
  type t = { id: Uuid.t
           ; name: string
           }
  let create ~id ~name = { id; name }
  let id self = self.id
  let name self = self.name
  let to_string self = Printf.sprintf "(%s) End:  %s"
      (Uuid.to_string self.id)
      self.name
end(* }}}*)
module XORGateway : sig(* {{{*)
  type t
  val create: id:Uuid.t -> name:string -> [`Start | `End] -> t
  val id: t -> Uuid.t
  val name: t -> string
  val is_start: t -> bool
  val is_end: t -> bool
  val to_string: t -> string
end = struct
  type t = { id: Uuid.t
           ; name: string
           ; start_or_end : [`Start | `End]
           }
  let create ~id ~name start_or_end = { id; name; start_or_end }
  let id self = self.id
  let name self = self.name
  let is_start self = self.start_or_end = `Start
  let is_end self = self.start_or_end = `End
  let to_string self =
    let type_str = match self.start_or_end with
      | `Start -> "Start"
      | `End -> "End"
    in
    Printf.sprintf "(%s) XOR (%s): %s"
      (Uuid.to_string self.id)
      type_str
      self.name
end(* }}}*)
module ANDGateway : sig(* {{{*)
  type t
  val create: id:Uuid.t -> name:string -> [`Start | `End] -> t
  val id: t -> Uuid.t
  val name: t -> string
  val is_start: t -> bool
  val is_end: t -> bool
  val to_string: t -> string
end = struct
  type t = { id: Uuid.t
           ; name: string
           ; start_or_end : [`Start | `End]
           }
  let create ~id ~name start_or_end = { id; name; start_or_end }
  let id self = self.id
  let name self = self.name
  let is_start self = self.start_or_end = `Start
  let is_end self = self.start_or_end = `End
  let to_string self =
    let type_str = match self.start_or_end with
      | `Start -> "Start"
      | `End -> "End"
    in
    Printf.sprintf "(%s) AND (%s): %s"
      (Uuid.to_string self.id)
      type_str
      self.name
end(* }}}*)

let to_uuid (id : string) = (String.drop_prefix id 4 |> Uuid.of_string)
let from_uuid (uuid : Uuid.t) = "sid-" ^ (Uuid.to_string uuid)

let level_str level = List.range 0 level |> List.fold ~init:"" ~f:(fun state x -> state ^ " ")

let merge_unique (l1 : 'a list) (l2 : 'a list) =
  List.fold ~init:l1 ~f:(fun state x ->
      if List.exists state ~f:(fun y -> x = y)
      then state
      else x :: state
    ) l2

module type RPST_intf = sig(* {{{ *)
  type node_t
  type fragment_t
  type t
  val calculate : node_t -> t
  val fragments : t -> fragment_t list
  val fragment_to_string: fragment_t -> string
  val fragment_private_activity_count: fragment_t -> int
  val extract_nodes: fragment_t -> (node_t * int * int * [`add_fragment | `pub | `priv]) list
  val to_string : t -> string
  val fragment_count : t -> int
end
(* }}} *)
module type NodeMap = sig(* {{{ *)
  type t
  type node_t

  val reindex  : t -> node_t option -> t * node_t option
  val add      : uuid:string -> node_t -> t -> t
  val empty    : t
  (*val traverse : t -> node_t -> init:'a -> f:('a -> int -> node_t -> 'a) -> 'a*)
end
(* }}} *)

module Make_Traversable (Node : Node)(* {{{ *)
  : Traversable with type node_t = Node.t
= struct
  type node_t = Node.t

  let rec traverse' node level ~init ~f =(* {{{*)
    match Node.traverse_class node with
    | `maybe_successor (x, next_level, this_level) -> maybe_traverse_child x (level + next_level) (f init (level + this_level) node) f
    | `stop                                        -> f init level node
    | `list_successors (x, next_level, this_level) -> list_traverse_children x (level + next_level) (f init (level + this_level) node) f
  (* }}}*)
  and maybe_traverse_child node level state f =(* {{{*)
    match node with
      | None -> state
      | Some next_node -> traverse' next_node level ~init:state ~f:f
  (* }}}*)
  and list_traverse_children nodes level state f =(* {{{*)
    List.fold ~init:state ~f:(fun next_state child ->
        traverse' child level ~init:next_state ~f:f)
      nodes
  (* }}}*)
  let traverse node ~init ~f = traverse' node 1 ~init:init ~f:f

  let exists xs x = List.exists ~f:(fun y -> y = x) xs

  let rec bfs' next_nodes visited level ~init ~f =(* {{{ *)
    match next_nodes with
    | [] -> init
    | (node, node_level) :: rest ->
      let visited' = (node, node_level) :: visited in
      match Node.traverse_class node with
      | `maybe_successor (x, next_level, this_level) ->
        let next_nodes' = maybe_traverse_child_bfs rest visited' x (node_level + next_level) in
        bfs' next_nodes' visited' (node_level + next_level) ~init:(f init (node_level + this_level) node) ~f:f
      | `stop ->
        bfs' rest visited' level ~init:(f init node_level node) ~f:f
      | `list_successors (x, next_level, this_level) ->
        let next_nodes' = list_traverse_children_bfs rest visited' x (node_level + next_level) in
        bfs' next_nodes' visited' (node_level + next_level) ~init:(f init (node_level + this_level) node) ~f:f
  (* }}} *)
  and maybe_traverse_child_bfs next_nodes visited node level =(* {{{ *)
    match node with
    | None -> next_nodes
    | Some node' ->
      if exists next_nodes (node', level) || exists visited (node', level)
      then next_nodes
      else let _ = printf "IN MAYBE ADD TO BFS LIST: %s\n" (Node.to_string node') in
        next_nodes @ [(node', level)]
        (*(node', level) :: next_nodes*)
  (* }}} *)
  and list_traverse_children_bfs next_nodes visited nodes level =(* {{{ *)
    let _ = printf "IN LIST ADD ------ \n" in
    let next_nodes' = List.fold ~init:[] ~f:(fun acc x ->
        if exists next_nodes (x, level) || exists visited (x, level)
        then acc else
          let _ = printf "IN LIST ADD TO BFS LIST: %s\n" (Node.to_string x) in
          acc @ [(x, level)])
        nodes
    in
    next_nodes' @ next_nodes
  (* }}} *)
  let bfs node ~init ~f = bfs' [(node, 1)] [] 1 ~init:init ~f:f

  let to_string node =(* {{{ *)
    traverse node ~init:"" ~f:(fun state level next_node ->
        Printf.sprintf "%s%s%s\n"
          state
          (level_str (level * 2))
          (Node.to_string next_node))
  (* }}} *)

end
(* }}} *)
module Make_RPST (Node : Node) (T : Traversable with type node_t = Node.t)(* {{{ *)
  : RPST_intf with type node_t = Node.t
= struct
  type node_t = Node.t
  (* TODO: maybe make this its own module, so api users have access to fragment details *)
  type fragment_t = { id: int
                    ; level: int
                    ; start_node: node_t
                    ; end_node: node_t option
                    }
  type t = { fragments: fragment_t list
           }

  type rpst_traversal_state = { counter: int
                              ; frags: fragment_t list
                              }

  let rec fragment_insert_end_node' xs level start_node_type node acc =(* {{{*)
    match xs with
    | [] -> List.rev acc
    | x :: xs' ->
      let expected_type = Node.rpst_class x.start_node in
      if x.level = level && expected_type = start_node_type
      then fragment_insert_end_node' xs' level start_node_type node @@ { x with end_node = Some node } :: acc
      else fragment_insert_end_node' xs' level start_node_type node @@ x :: acc
  (* }}}*)
  let fragment_insert_end_node xs level start_node node =(* {{{*)
    fragment_insert_end_node' xs level start_node node []
  (* }}}*)
  let calculate start_node =(* {{{ *)
    let new_state = T.traverse start_node
      ~init:{ counter = 0
            ; frags = []
            }
      ~f:(fun state level node ->
          match Node.rpst_fragment_class node with
          | `add_fragment ->
            let new_fragment = { id = state.counter
                               ; level = level
                               ; start_node = node
                               ; end_node = None
                               }
            in
            { counter = state.counter + 1
            ; frags = new_fragment :: state.frags
            }
          | `close_start_node ->
            { state with frags = fragment_insert_end_node state.frags level `start_node node }
          | `pub -> state
          | `priv -> state
          | `close_xor_node ->
            { state with frags = fragment_insert_end_node state.frags level `xor_node node }
          | `close_and_node ->
            { state with frags = fragment_insert_end_node state.frags level `and_node node }
        )
  in
  { fragments = List.rev new_state.frags }
  (* }}} *)
  let fragments self = self.fragments
  let fragment_count self = List.length self.fragments

  let fragment_to_string fragment = (* {{{ *)
    Printf.sprintf "id: %d, level: %d, start node: %s, end node: %s"
      fragment.id
      fragment.level
      (Node.to_string fragment.start_node)
      (match fragment.end_node with
       | None -> ""
       | Some node -> Node.to_string node)
  (* }}}*)
  let extract_nodes fragment =(* {{{*)
    let node_exists xs x = List.exists xs ~f:(fun y -> x = y) in
    T.traverse fragment.start_node
      ~init:[]
      ~f:(fun state level node ->
          match Node.rpst_fragment_class node with
          | `add_fragment ->
            let x = (node, fragment.level, level, `add_fragment) in
            if not (node_exists state x)
            then x :: state
            else state
          | `pub ->
            let x = (node, fragment.level, level, `pub) in
            if fragment.level = level && not (node_exists state x)
            then x :: state
            else state
          | `priv ->
            let x = (node, fragment.level, level, `priv) in
            if fragment.level = level && not (node_exists state x)
            then x :: state
            else state
          | _ -> state
        ) |> List.rev
(* }}}*)
  let to_string state =(* {{{ *)
    List.fold
      ~init:""
      ~f:(fun state fragment ->
          let print_nodes xs = List.fold ~init:""
              ~f:(fun state (node, fragment_level, level, typ) ->
                  Printf.sprintf "%s\n\t%s (fragment level: %d) (level: %d)"
                    state
                    (Node.to_string node)
                    fragment_level
                    level
                )
              xs
          in
          Printf.sprintf "%s\n%s\n\t%s"
            state
            (fragment_to_string fragment)
            (print_nodes (extract_nodes fragment))
        )
      state.fragments
  (* }}} *)
  let fragment_private_activity_count fragment = (* {{{ *)
    let nodes = List.filter ~f:(fun (_, _, _, typ) -> typ = `priv) (extract_nodes fragment) in
    List.length nodes
  (* }}} *)
end
(* }}} *)
module Make_NodeMap (N : Node)(* {{{ *)
  : NodeMap with type node_t = N.t
             and type t = N.t String.Map.t
= struct

  type node_t = N.t
  type t = node_t String.Map.t

  let maybe_uuid maybe_node =(* {{{*)
    match maybe_node with
    | None -> failwith "non-existent public node for uuid lookup"
    | Some node -> N.id node
  (* }}}*)
  let rec update_outgoing self start_uuid current_uuid =(* {{{*)
    (*printf "update_outgoing: UUID=%s" current_uuid;*)
    let node' = match Map.find self current_uuid with
    | None -> failwith @@ "non-existent node with id: " ^ current_uuid
    | Some node' -> node'
    in
    match N.update_node_map node' with
    | `no_update -> self, None
    | `update_single x ->
      let self', next_key = handle_simple node' x self start_uuid in
      let outgoing = match Map.find self' next_key with
        | None -> []
        | Some outgoing -> [outgoing]
      in
      let updated_node = N.update_outgoing outgoing node' in
      Map.add self' ~key:current_uuid ~data:updated_node, None
    | `update_multi x -> let self', successors = handle_list node' x self start_uuid in
      let updated_node = N.update_outgoing successors node' in
      Map.add self' ~key:current_uuid ~data:updated_node, None
  (* }}}*)
  and handle_simple node maybe_next_node self start_uuid =(* {{{*)
    (*printf "IN: %s\n" @@ N.to_string node;*)
    let next_key = maybe_uuid maybe_next_node in
    (*printf "NEXT KEY: %s\n" next_key;*)
    let self', _ = reindex' self start_uuid @@ Some next_key in
    self', next_key
  (* }}}*)
  and handle_list node' next_nodes self start_uuid =(* {{{*)
    (*printf "IN: %s\n" @@ N.to_string node';*)
    let self', successor_uuids = List.fold ~init:(self, [])
        ~f:(fun (state, successor_acc) successor ->
            let next_key = N.id successor in
            (*printf "NEXT KEY: %s\n" next_key;*)
            let state', _ = reindex' state start_uuid @@ Some next_key in
            state', next_key :: successor_acc
          ) next_nodes in
    let successors' = List.fold ~init:[] ~f:(fun state uuid ->
        match Map.find self' uuid with
        | None -> state
        | Some x -> x :: state)
        successor_uuids
    in self', successors'
(* }}}*)
  and reindex' self start_uuid maybe_current_uuid =(* {{{*)
    match maybe_current_uuid with
    | None -> (self, Map.find self start_uuid)
    | Some current_uuid -> update_outgoing self start_uuid current_uuid
  (* }}}*)
  let reindex self maybe_start_node =(* {{{*)

    (* the nodes map is at this point complete, need to reindex all the
       outgoing nodes to the fully mapped ones. Use the uuids to look the
       correct nodes *)

    let uuid = match maybe_start_node with
      | None -> ""
      | Some start_node -> N.id start_node
    in
    if uuid = ""
       then self, None
       else let self', _ = reindex' self uuid (Some uuid)
         in self', Map.find self' uuid
         (* }}}*)

  let add ~uuid target self =(* {{{*)
    Map.add self ~key:uuid ~data:target
  (* }}}*)
  let empty = String.Map.empty(* {{{*)
  (* }}}*)
end
(* }}} *)

module type NodeMapTransform = sig(* {{{ *)

  type t
  type source_t
  type target_t

  val transform :    is_eligable_node:(source_t -> bool)
                  -> transform:(source_t -> target_t)
                  -> get_successors:( is_eligable_node:(source_t -> bool) -> transform:(source_t -> target_t) -> source_t -> target_t list )
                  -> source_t
                  -> t
  val start_node: t -> target_t
  val node_map: t -> target_t String.Map.t
end
(* }}} *)
module Make_NodeMapTransform(* {{{ *)
    (S   : Node)
    (ST  : Traversable with type node_t = S.t)
    (T   : Node)
    (TT  : Traversable with type node_t = T.t)
    (TNM : NodeMap with type node_t = T.t
                    and type t = T.t String.Map.t)
  : NodeMapTransform with type source_t = S.t
                      and type target_t = T.t
= struct

  type source_t = S.t
  type target_t = T.t

  type t = { nodes: target_t String.Map.t
           ; graph: target_t
           }

  let update_target_nodes ~is_eligable_node ~transform ~get_successors target_nodes node =(* {{{*)
    (* build up the public node mapping here, but only if this chor_node is eligable *)
    if is_eligable_node node
    then
      let target_node = match Map.find target_nodes (S.id node) with
        | None   -> transform node
        | Some x -> x
      in
      let successors = get_successors ~is_eligable_node:is_eligable_node ~transform:transform node in
      let target_node' = T.add_outgoing successors target_node in
      Map.add target_nodes ~key:(T.id target_node) ~data:target_node'
    else target_nodes
  (* }}}*)
  let transform ~is_eligable_node ~transform ~get_successors node =(* {{{ *)
    let target_nodes, start_node = ST.traverse node
        ~init:(String.Map.empty, None)
        ~f:(fun (target_nodes, start_node) level curr_node ->
            let target_nodes' = update_target_nodes ~is_eligable_node:is_eligable_node ~transform:transform ~get_successors:get_successors target_nodes curr_node in
            let start_node' = if S.is_start curr_node
              then Some (transform curr_node)
              else start_node
            in
            (target_nodes', start_node'))
    in
    let target_nodes', start_node' = TNM.reindex target_nodes start_node in
    (*print_endline @@ Map.fold ~init:""*)
      (*~f:(fun ~key ~data state ->*)
          (*Printf.sprintf "%skey:%s -> node:%s\n"*)
            (*state*)
            (*key*)
            (*(TT.to_string data)) target_nodes';*)
    match start_node' with
    | None -> failwith "no start node found"
    | Some start_node' ->
      (*print_endline "THE WHOLE MODEL:";*)
      (*print_endline @@ TT.to_string start_node';*)
      { nodes = target_nodes'
      ; graph = start_node'
      }
  (* }}} *)
  let start_node self = self.graph
  let node_map self = self.nodes

end
(* }}} *)

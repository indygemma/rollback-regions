open Core.Std

module type Node = sig(* {{{*)
  type t
  val to_string : t -> string
  val id : t -> string

  val rpst_class : t -> [`start_node | `xor_node | `and_node | `rest]
  val rpst_fragment_class : t -> [`add_fragment | `close_start_node | `close_xor_node | `close_and_node | `none]
  val traverse_class : t -> [`maybe_successor of t option * int * int | `list_successors of t list * int * int | `stop]

  val add_outgoing: t list -> t -> t
  val empty_start: t option
  val choose_start: t -> t option -> t option
  val is_start: t -> bool
end
(* }}}*)
module type Traversable = sig(* {{{ *)
  type node_t
  val traverse: node_t -> init:('a) -> f:('a -> int -> node_t -> 'a) -> 'a
  val to_string: node_t -> string
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
  val to_string : t -> string
end
(* }}} *)

module Make_Traversable (Node : Node)(* {{{ *)
  : Traversable with type node_t = Node.t
= struct
  type node_t = Node.t

  let rec traverse' node level ~init ~f =(* {{{*)
    match Node.traverse_class node with
    | `maybe_successor (x, next_level, this_level) -> maybe_traverse_child x (level + next_level) (f init (level + this_level) node) f
    | `stop              -> f init level node
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
          | `none -> state
          | `close_xor_node ->
            { state with frags = fragment_insert_end_node state.frags level `xor_node node }
          | `close_and_node ->
            { state with frags = fragment_insert_end_node state.frags level `and_node node }
        )
  in
  { fragments = List.rev new_state.frags }
  (* }}} *)
  let fragments self = self.fragments

  let fragment_to_string fragment = (* {{{ *)
    Printf.sprintf "id: %d, level: %d, start node: %s, end node: %s"
      fragment.id
      fragment.level
      (Node.to_string fragment.start_node)
      (match fragment.end_node with
       | None -> ""
       | Some node -> Node.to_string node)
  (* }}}*)
  let to_string state =(* {{{ *)
    List.fold
      ~init:""
      ~f:(fun state fragment -> state ^ "\n" ^ (fragment_to_string fragment))
      state.fragments
  (* }}} *)
end
(* }}} *)

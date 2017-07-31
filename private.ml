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
open Common

module PrivateActivity : sig(* {{{*)
  type t
  val create: id:Uuid.t -> name:string -> role:string -> t
  val id: t -> Uuid.t
  val role: t -> string
  val name: t -> string
  val to_string: t -> string
end = struct
  type t = { id: Uuid.t
           ; name: string
           ; role: string
           }
  let create ~id ~name ~role =
    { id
    ; name
    ; role
    }

  let id self = self.id
  let role self = self.role
  let name self = self.name
  let to_string self =
    Printf.sprintf "(%s) Private Activity: %s. Role: %s"
      (Uuid.to_string self.id)
      self.name
      self.role

end(* }}}*)

type private_node = StartNode of { start_node: StartEvent.t ; outgoing: private_node option }
                  | EndNode of { end_node: EndEvent.t }
                  | SendNode of { node: Public.SendActivity.t ; outgoing: private_node option }
                  | ReceiveNode of { node: Public.ReceiveActivity.t ; outgoing: private_node option }
                  | PrivateNode of { node: PrivateActivity.t; outgoing: private_node option }
                  | XORGatewayStartNode of { xor: XORGateway.t ; outgoing: private_node list }
                  | XORGatewayEndNode of { xor: XORGateway.t ; outgoing: private_node option }
                  | ANDGatewayStartNode of { par: ANDGateway.t ; outgoing: private_node list }
                  | ANDGatewayEndNode of { par: ANDGateway.t ; outgoing: private_node option }

module PrivateNode : Node with type t = private_node = struct(* {{{ *)
  type t = private_node

  let rpst_class = function(* {{{ *)
    | StartNode _           -> `start_node
    | XORGatewayStartNode _ -> `xor_node
    | ANDGatewayStartNode _ -> `and_node
    | _                     -> `rest
  (* }}} *)
  let rpst_fragment_class = function(* {{{ *)
    | StartNode _           -> `add_fragment
    | EndNode _             -> `close_start_node
    | SendNode _            -> `pub
    | ReceiveNode _         -> `pub
    | PrivateNode _         -> `priv
    | XORGatewayStartNode _ -> `add_fragment
    | XORGatewayEndNode _   -> `close_xor_node
    | ANDGatewayStartNode _ -> `add_fragment
    | ANDGatewayEndNode _   -> `close_and_node
  (* }}} *)
  let traverse_class = function(* {{{ *)
    | StartNode x           -> `maybe_successor (x.outgoing,  0, 0)
    | EndNode x             -> `stop
    | SendNode x            -> `maybe_successor (x.outgoing,  0, 0)
    | ReceiveNode x         -> `maybe_successor (x.outgoing,  0, 0)
    | PrivateNode x         -> `maybe_successor (x.outgoing,  0, 0)
    | XORGatewayStartNode x -> `list_successors (x.outgoing,  1, 1)
    | XORGatewayEndNode x   -> `maybe_successor (x.outgoing, -1, 0)
    | ANDGatewayStartNode x -> `list_successors (x.outgoing,  1, 1)
    | ANDGatewayEndNode x   -> `maybe_successor (x.outgoing, -1, 0)
  (* }}} *)
  let update_node_map = function(* {{{ *)
    | StartNode x           -> `update_single x.outgoing
    | EndNode x             -> `no_update
    | SendNode x            -> `update_single x.outgoing
    | ReceiveNode x         -> `update_single x.outgoing
    | PrivateNode x         -> `update_single x.outgoing
    | XORGatewayStartNode x -> `update_multi x.outgoing
    | XORGatewayEndNode x   -> `update_single x.outgoing
    | ANDGatewayStartNode x -> `update_multi x.outgoing
    | ANDGatewayEndNode x   -> `update_single x.outgoing
  (* }}} *)
  let id node =(* {{{*)
    match node with
    | StartNode x -> StartEvent.id x.start_node |> from_uuid
    | EndNode x -> EndEvent.id x.end_node |> from_uuid
    | SendNode x -> Public.SendActivity.id x.node |> from_uuid
    | ReceiveNode x -> Public.ReceiveActivity.id x.node |> from_uuid
    | PrivateNode x -> PrivateActivity.id x.node |> from_uuid
    | XORGatewayStartNode x -> XORGateway.id x.xor |> from_uuid
    | XORGatewayEndNode x -> XORGateway.id x.xor |> from_uuid
    | ANDGatewayStartNode x -> ANDGateway.id x.par |> from_uuid
    | ANDGatewayEndNode x -> ANDGateway.id x.par |> from_uuid
    (* }}}*)
  let to_string node =(* {{{*)
    match node with
    | StartNode x -> (StartEvent.to_string x.start_node)
    | EndNode x -> (EndEvent.to_string x.end_node)
    | SendNode x -> (Public.SendActivity.to_string x.node)
    | ReceiveNode x -> (Public.ReceiveActivity.to_string x.node)
    | PrivateNode x -> (PrivateActivity.to_string x.node)
    | XORGatewayStartNode x -> (XORGateway.to_string x.xor)
    | XORGatewayEndNode x -> (XORGateway.to_string x.xor)
    | ANDGatewayStartNode x -> (ANDGateway.to_string x.par)
    | ANDGatewayEndNode x -> (ANDGateway.to_string x.par)
  (* }}}*)

  let add_outgoing target source =(* {{{*)
    match source with
    | StartNode x           -> StartNode { x with outgoing = List.hd target }
    | EndNode x             -> EndNode x
    | SendNode x            -> SendNode { x with outgoing = List.hd target }
    | ReceiveNode x         -> ReceiveNode { x with outgoing = List.hd target }
    | PrivateNode x         -> PrivateNode { x with outgoing = List.hd target }
    | XORGatewayStartNode x -> XORGatewayStartNode { x with outgoing = merge_unique target x.outgoing }
    | XORGatewayEndNode x   -> XORGatewayEndNode { x with outgoing = List.hd target }
    | ANDGatewayStartNode x -> ANDGatewayStartNode { x with outgoing = merge_unique target x.outgoing }
    | ANDGatewayEndNode x   -> ANDGatewayEndNode { x with outgoing = List.hd target }
  (* }}}*)
  let update_outgoing target source =(* {{{*)
    match source with
    | StartNode x           -> StartNode { x with outgoing = List.hd target }
    | EndNode x             -> EndNode x
    | SendNode x            -> SendNode { x with outgoing = List.hd target }
    | ReceiveNode x         -> ReceiveNode { x with outgoing = List.hd target }
    | PrivateNode x         -> PrivateNode { x with outgoing = List.hd target }
    | XORGatewayStartNode x -> XORGatewayStartNode { x with outgoing = target }
    | XORGatewayEndNode x   -> XORGatewayEndNode { x with outgoing = List.hd target }
    | ANDGatewayStartNode x -> ANDGatewayStartNode { x with outgoing = target }
    | ANDGatewayEndNode x   -> ANDGatewayEndNode { x with outgoing = List.hd target }
  (* }}}*)
  let empty_start = None
  let is_start node = match node with(* {{{ *)
    | StartNode _ -> true
    | _ -> false
(* }}} *)
  let choose_start left maybe_right =(* {{{*)
    match (is_start left, maybe_right) with
    | (false, None      ) -> None
    | (false, Some right) -> if is_start right then Some right else None
    | (true, _          ) -> Some left
  (* }}}*)
end

(* }}} *)

module PrivateTraverse = Make_Traversable (PrivateNode)
module PrivateRPST = Make_RPST (PrivateNode) (PrivateTraverse)
module PrivateNodeMap = Make_NodeMap (PrivateNode)

module PublicPrivateTransform = Make_NodeMapTransform
    (Public.PublicNode)
    (Public.PublicTraverse)
    (PrivateNode)
    (PrivateTraverse)
    (PrivateNodeMap)

let is_eligable_node = function(* {{{*)
  | Public.StartNode _ -> true
  | Public.EndNode _   -> true
  | Public.SendNode _ -> true
  | Public.ReceiveNode _ -> true
  | Public.XORGatewayStartNode _ -> true
  | Public.XORGatewayEndNode _   -> true
  | Public.ANDGatewayStartNode _ -> true
  | Public.ANDGatewayEndNode _   -> true
(* }}}*)
let public_to_private_transform_node = function(* {{{*)
  | Public.StartNode x -> StartNode { start_node = x.start_node
                                    ; outgoing = None
                                    }
  | Public.EndNode x -> EndNode { end_node = x.end_node }
  | Public.SendNode x -> SendNode { node = x.node
                                  ; outgoing = None
                                  }
  | Public.ReceiveNode x -> ReceiveNode { node = x.node
                                        ; outgoing = None
                                        }
  | Public.XORGatewayStartNode x -> XORGatewayStartNode { xor = x.xor
                                                        ; outgoing = []
                                                        }
  | Public.XORGatewayEndNode x -> XORGatewayEndNode { xor = x.xor
                                                    ; outgoing = None
                                                    }
  | Public.ANDGatewayStartNode x -> ANDGatewayStartNode { par = x.par
                                                        ; outgoing = []
                                                        }
  | Public.ANDGatewayEndNode x -> ANDGatewayEndNode { par = x.par
                                                    ; outgoing = None
                                                    }
(* }}}*)
let rec find_eligable_successors ~is_eligable_node ~transform node =(* {{{*)
  match node with
  | Public.StartNode x           -> maybe_find_eligable_successor ~is_eligable_node:is_eligable_node ~transform:transform x.outgoing
  | Public.EndNode x             -> []
  | Public.SendNode x            -> maybe_find_eligable_successor ~is_eligable_node:is_eligable_node ~transform:transform x.outgoing
  | Public.ReceiveNode x         -> maybe_find_eligable_successor ~is_eligable_node:is_eligable_node ~transform:transform x.outgoing
  | Public.XORGatewayStartNode x -> collect_eligable_succesors    ~is_eligable_node:is_eligable_node ~transform:transform x.outgoing
  | Public.XORGatewayEndNode x   -> maybe_find_eligable_successor ~is_eligable_node:is_eligable_node ~transform:transform x.outgoing
  | Public.ANDGatewayStartNode x -> collect_eligable_succesors    ~is_eligable_node:is_eligable_node ~transform:transform x.outgoing
  | Public.ANDGatewayEndNode x   -> maybe_find_eligable_successor ~is_eligable_node:is_eligable_node ~transform:transform x.outgoing
(* }}}*)
and maybe_find_eligable_successor ~is_eligable_node ~transform maybe_chor_node =(* {{{*)
  match maybe_chor_node with
  | None -> []
  | Some next_node -> if is_eligable_node next_node
    then [(transform next_node)]
    else find_eligable_successors ~is_eligable_node:is_eligable_node ~transform:transform next_node
(* }}}*)
and collect_eligable_succesors ~is_eligable_node ~transform next_nodes =(* {{{*)
  List.fold ~init:[] ~f:(fun state next_node ->
      if is_eligable_node next_node
      then
        let public_node = transform next_node in
        if List.exists state ~f:(fun node -> node = public_node)
        then state
        else public_node :: state
      else find_eligable_successors ~is_eligable_node:is_eligable_node ~transform:transform next_node @ state)
    next_nodes
  |> List.rev
(* }}}*)

let graph_public_to_private_transform node =
  PublicPrivateTransform.transform
    ~is_eligable_node:is_eligable_node
    ~transform:public_to_private_transform_node
    ~get_successors:find_eligable_successors
    node

let add_random_private_activity_to_fragment start_node role nodemap frag current_id =
    (* create a random private activity: new uuid + name = "private {current_id}" *)
    let new_uuid = Uuid.create () in
    let name = Printf.sprintf "private #%d" current_id in
    let new_node = PrivateNode { node = PrivateActivity.create new_uuid name role; outgoing = None } in
    let nodes = PrivateRPST.extract_nodes frag in
    let arr = Array.of_list nodes in
    let n = Random.int (Array.length arr) in
    let (node, _, _, typ) = Array.get arr n in
    match PrivateNode.traverse_class node with
    | `list_successors (succ, _, _) ->
        (* XOR or AND *)
        (* pick random outgoing node *)
        let arr = Array.of_list succ in
        let n = Random.int (Array.length arr) in
        let picked_node = Array.get arr n in
        printf "picked node (XOR|AND): %s\n" (PrivateNode.id picked_node);
        let succ' = List.filter ~f:(fun x -> x <> picked_node) succ in
        (* set picked uuid as new activity's outgoing *)
        let new_node' = PrivateNode.add_outgoing [picked_node] new_node in
        (* add new private activity to nodemap *)
        let nodemap' = PrivateNodeMap.add (from_uuid new_uuid) new_node' nodemap in
        (* replace picked uuid with the new activity's uuid *)
        let node' = PrivateNode.add_outgoing (new_node'::succ') node in
        let nodemap' = PrivateNodeMap.add (PrivateNode.id node) node' nodemap' in
        (* reindex nodemap *)
        (*printf "reindex #1 %s\n" (from_uuid new_uuid);*)
        PrivateNodeMap.reindex nodemap' (Some start_node)
    | `maybe_successor (Some succ, _, _) ->
        printf "picked node: %s" (PrivateNode.id succ);
        (* set picked uuid as new activity's outgoing *)
        let new_node' = PrivateNode.add_outgoing [succ] new_node in
        (* add new private activity to nodemap *)
        let nodemap' = PrivateNodeMap.add (from_uuid new_uuid) new_node' nodemap in
        (* replace picked uuid with the new activity's uuid *)
        let node' = PrivateNode.add_outgoing [new_node'] node in
        let nodemap' = PrivateNodeMap.add (PrivateNode.id node) node' nodemap' in
        (* reindex nodemap *)
        (*printf "reindex #2 %s\n" (from_uuid new_uuid);*)
        PrivateNodeMap.reindex nodemap' (Some start_node)
    | _ ->
        nodemap, Some start_node

let rec add_random_private_activity_to_fragments start_node role nodemap rpst count current_id=
    let (private_activity_count_diffs, frag_queue) = List.fold
        ~init:(0,[])
        ~f:(fun (diff_total, cur_queue) f ->
            let count' = PrivateRPST.fragment_private_activity_count f in
            let diff = count - count' in
            (diff_total + diff, if diff > 0 then f :: cur_queue else cur_queue))
        (PrivateRPST.fragments rpst)
    in
    if private_activity_count_diffs <> 0 then
        let f = List.hd_exn frag_queue in
        printf "Adding to fragment: %s\n" (PrivateRPST.fragment_to_string f);
        let nodemap', start_node' = add_random_private_activity_to_fragment start_node role nodemap f current_id in
        match start_node' with
        | Some sn ->
            let rpst' = PrivateRPST.calculate sn in
            add_random_private_activity_to_fragments sn role nodemap' rpst' count (current_id + 1)
        | None ->
            add_random_private_activity_to_fragments start_node role nodemap' rpst count (current_id + 1)
    else
        nodemap, start_node

let add_random_private_activities role count target_nodes =
    let start_node = PublicPrivateTransform.start_node target_nodes in
    let nodemap = PublicPrivateTransform.node_map target_nodes in
    let rpst = PrivateRPST.calculate start_node in
    (*let _ = List.map ~f:(fun f -> printf "%s\n" (PrivateRPST.fragment_to_string f)) (PrivateRPST.fragments rpst) in*)
    add_random_private_activity_to_fragments start_node role nodemap rpst count 0

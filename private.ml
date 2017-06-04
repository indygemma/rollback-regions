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

let public_to_private_transform node =(* {{{*)
  match node with
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

(* }}} *)
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
    | SendNode _            -> `none
    | ReceiveNode _         -> `none
    | PrivateNode _         -> `none
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

(* TODO: similar to projection function that turns all the public graph nodes to private nodes including relations *)

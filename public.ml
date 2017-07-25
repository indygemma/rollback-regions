open Core
open Common

module SendActivity : sig(* {{{*)
  type t
  val create: id:Uuid.t -> name:string -> role:string -> recipient:Participant.t -> message:Choreography.Message.t option -> t
  val of_interaction: Choreography.Interaction.t -> role:string -> t
  val id: t -> Uuid.t
  val role: t -> string
  val name: t -> string
  val message: t -> Choreography.Message.t option
  val recipient: t -> Participant.t
  val to_string: t -> string
end = struct
  type t = { id: Uuid.t
           ; name: string
           ; recipient: Participant.t
           ; message: Choreography.Message.t option
           ; role: string
           }
  let create ~id ~name ~role ~recipient ~message =
    { id
    ; name
    ; role
    ; recipient
    ; message
    }

  let of_interaction x ~role =
    let open Choreography.Interaction in
    { id = id x
    ; name = name x
    ; role = role
    ; recipient = receiver x
    ; message = message x
    }

  let id self = self.id
  let role self = self.role
  let name self = self.name
  let message self = self.message
  let recipient self = self.recipient
  let to_string self =
    let msg = match self.message with
      | None -> "(no message)"
      | Some msg' -> Choreography.Message.to_string msg'
    in
    Printf.sprintf "(%s) Send: %s. Role: %s -> Recipient: %s. Message: %s"
      (Uuid.to_string self.id)
      self.name
      self.role
      (Participant.to_string self.recipient)
      msg

end(* }}}*)
module ReceiveActivity : sig(* {{{*)
  type t
  val create: id:Uuid.t -> name:string -> role:string -> sender:Participant.t -> message:Choreography.Message.t option -> t
  val of_interaction: Choreography.Interaction.t -> role:string -> t
  val id: t -> Uuid.t
  val role: t -> string
  val name: t -> string
  val message: t -> Choreography.Message.t option
  val sender: t -> Participant.t
  val to_string: t -> string
end = struct
  type t = { id: Uuid.t
           ; name: string
           ; sender: Participant.t
           ; message: Choreography.Message.t option
           ; role: string
           }
  let create ~id ~name ~role ~sender ~message =
    { id
    ; name
    ; role
    ; sender
    ; message
    }

  let of_interaction x ~role =
    let open Choreography.Interaction in
    { id = id x
    ; name = name x
    ; role = role
    ; sender = sender x
    ; message = message x
    }

  let id self = self.id
  let name self = self.name
  let role self = self.role
  let message self = self.message
  let sender self = self.sender
  let to_string self =
    let msg = match self.message with
      | None -> "(no message)"
      | Some msg' -> Choreography.Message.to_string msg'
    in
    Printf.sprintf "(%s) Receive: %s. Sender: %s -> Role: %s. Message: %s"
      (Uuid.to_string self.id)
      self.name
      (Participant.to_string self.sender)
      self.role
      msg

end(* }}}*)

type public_node = StartNode           of { start_node: StartEvent.t ; outgoing: public_node option }
                 | EndNode             of { end_node: EndEvent.t }
                 | SendNode            of { node: SendActivity.t ; outgoing: public_node option }
                 | ReceiveNode         of { node: ReceiveActivity.t ; outgoing: public_node option }
                 | XORGatewayStartNode of { xor: XORGateway.t ; outgoing: public_node list }
                 | XORGatewayEndNode   of { xor: XORGateway.t ; outgoing: public_node option }
                 | ANDGatewayStartNode of { par: ANDGateway.t ; outgoing: public_node list }
                 | ANDGatewayEndNode   of { par: ANDGateway.t ; outgoing: public_node option }

module PublicNode : Node with type t = public_node = struct(* {{{ *)
  type t = public_node

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
    | XORGatewayStartNode x -> `list_successors (x.outgoing,  1, 0)
    | XORGatewayEndNode x   -> `maybe_successor (x.outgoing, -1, -1)
    | ANDGatewayStartNode x -> `list_successors (x.outgoing,  1, 0)
    | ANDGatewayEndNode x   -> `maybe_successor (x.outgoing, -1, -1)
  (* }}} *)
  let update_node_map = function(* {{{ *)
    | StartNode x           -> `update_single x.outgoing
    | EndNode x             -> `no_update
    | SendNode x            -> `update_single x.outgoing
    | ReceiveNode x         -> `update_single x.outgoing
    | XORGatewayStartNode x -> `update_multi x.outgoing
    | XORGatewayEndNode x   -> `update_single x.outgoing
    | ANDGatewayStartNode x -> `update_multi x.outgoing
    | ANDGatewayEndNode x   -> `update_single x.outgoing
  (* }}} *)
  let id node =(* {{{*)
    match node with
    | StartNode x -> StartEvent.id x.start_node |> from_uuid
    | EndNode x -> EndEvent.id x.end_node |> from_uuid
    | SendNode x -> SendActivity.id x.node |> from_uuid
    | ReceiveNode x -> ReceiveActivity.id x.node |> from_uuid
    | XORGatewayStartNode x -> XORGateway.id x.xor |> from_uuid
    | XORGatewayEndNode x -> XORGateway.id x.xor |> from_uuid
    | ANDGatewayStartNode x -> ANDGateway.id x.par |> from_uuid
    | ANDGatewayEndNode x -> ANDGateway.id x.par |> from_uuid
    (* }}}*)
  let to_string node =(* {{{*)
    match node with
    | StartNode x -> (StartEvent.to_string x.start_node)
    | EndNode x -> (EndEvent.to_string x.end_node)
    | SendNode x -> (SendActivity.to_string x.node)
    | ReceiveNode x -> (ReceiveActivity.to_string x.node)
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

module PublicTraverse = Make_Traversable (PublicNode)
module PublicRPST = Make_RPST (PublicNode) (PublicTraverse)
module PublicNodeMap = Make_NodeMap (PublicNode)

module ChorPublicTransform = Make_NodeMapTransform
    (Choreography.ChoreographyNode)
    (Choreography.ChoreographyTraverse)
    (PublicNode)
    (PublicTraverse)
    (PublicNodeMap)

let is_eligable_node by_role chor_node =(* {{{*)
  match chor_node with
  | Choreography.StartNode _ -> true
  | Choreography.EndNode _   -> true
  | Choreography.InteractionNode x ->
    if (Choreography.Interaction.sender   x.interaction |> Participant.name) = by_role
    || (Choreography.Interaction.receiver x.interaction |> Participant.name) = by_role
    then true
    else false
  | Choreography.XORGatewayStartNode _ -> true
  | Choreography.XORGatewayEndNode _   -> true
  | Choreography.ANDGatewayStartNode _ -> true
  | Choreography.ANDGatewayEndNode _   -> true
(* }}}*)
let choreography_to_public_transform ~role node =(* {{{*)
match node with
| Choreography.StartNode x -> StartNode { start_node = x.start_node
                                        ; outgoing = None
                                        }
| Choreography.EndNode x -> EndNode { end_node = x.end_node }
| Choreography.InteractionNode x ->
  if role = (Choreography.Interaction.sender x.interaction |> Participant.name)
  then SendNode { node = SendActivity.of_interaction x.interaction ~role:role
                ; outgoing = None
                }
  else ReceiveNode { node = ReceiveActivity.of_interaction x.interaction ~role:role
                   ; outgoing = None
                   }
| Choreography.XORGatewayStartNode x -> XORGatewayStartNode { xor = x.xor
                                                            ; outgoing = []
                                                            }
| Choreography.XORGatewayEndNode x -> XORGatewayEndNode { xor = x.xor
                                                        ; outgoing = None
                                                        }
| Choreography.ANDGatewayStartNode x -> ANDGatewayStartNode { par = x.par
                                                            ; outgoing = []
                                                            }
| Choreography.ANDGatewayEndNode x -> ANDGatewayEndNode { par = x.par
                                                        ; outgoing = None
                                                        }
(* }}}*)
let rec find_eligable_successors by_role ~is_eligable_node ~transform node =(* {{{*)
match node with
| Choreography.StartNode x           -> maybe_find_eligable_successor ~is_eligable_node:is_eligable_node ~transform:transform x.outgoing by_role
| Choreography.EndNode x             -> []
| Choreography.InteractionNode x     -> maybe_find_eligable_successor ~is_eligable_node:is_eligable_node ~transform:transform x.outgoing by_role
| Choreography.XORGatewayStartNode x -> collect_eligable_succesors    ~is_eligable_node:is_eligable_node ~transform:transform x.outgoing by_role
| Choreography.XORGatewayEndNode x   -> maybe_find_eligable_successor ~is_eligable_node:is_eligable_node ~transform:transform x.outgoing by_role
| Choreography.ANDGatewayStartNode x -> collect_eligable_succesors    ~is_eligable_node:is_eligable_node ~transform:transform x.outgoing by_role
| Choreography.ANDGatewayEndNode x   -> maybe_find_eligable_successor ~is_eligable_node:is_eligable_node ~transform:transform x.outgoing by_role
(* }}}*)
and maybe_find_eligable_successor ~is_eligable_node ~transform maybe_chor_node by_role =(* {{{*)
match maybe_chor_node with
| None -> []
| Some next_node -> if is_eligable_node next_node
  then [(transform next_node)]
  else find_eligable_successors by_role ~is_eligable_node:is_eligable_node ~transform:transform next_node
(* }}}*)
and collect_eligable_succesors ~is_eligable_node ~transform next_nodes by_role =(* {{{*)
List.fold ~init:[] ~f:(fun state next_node ->
    if is_eligable_node next_node
    then
      let public_node = transform next_node in
      if List.exists state ~f:(fun node -> node = public_node)
      then state
      else public_node :: state
    else find_eligable_successors by_role ~is_eligable_node:is_eligable_node ~transform:transform next_node @ state)
  next_nodes
|> List.rev
(* }}}*)

let graph_choreography_to_public_transform chor_node ~role =
  ChorPublicTransform.transform
    ~is_eligable_node:(is_eligable_node role)
    ~transform:(choreography_to_public_transform ~role:role)
    ~get_successors:(find_eligable_successors role)
    chor_node

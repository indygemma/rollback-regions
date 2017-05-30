open Core.Std
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

type public_node = StartNode of { start_node: StartEvent.t ; outgoing: public_node option }
                 | EndNode of { end_node: EndEvent.t }
                 | SendNode of { node: SendActivity.t ; outgoing: public_node option }
                 | ReceiveNode of { node: ReceiveActivity.t ; outgoing: public_node option }
                 | XORGatewayStartNode of { xor: XORGateway.t ; outgoing: public_node list }
                 | XORGatewayEndNode of { xor: XORGateway.t ; outgoing: public_node option }
                 | ANDGatewayStartNode of { par: ANDGateway.t ; outgoing: public_node list }
                 | ANDGatewayEndNode of { par: ANDGateway.t ; outgoing: public_node option }

module PublicNode : sig
  val id: public_node -> string
  val to_string: public_node -> string
  val to_string_deep: public_node -> string
  val of_choreography_node: Choreography.choreography_node -> role:string -> public_node
  val traverse: public_node -> init:'a -> f:('a -> int -> public_node -> 'a) -> 'a
  val add_outgoing: public_node -> public_node list -> public_node
end = struct
  let id node =(* {{{*)
    let uuid = match node with
    | StartNode x           -> StartEvent.id x.start_node
    | EndNode x             -> EndEvent.id x.end_node
    | SendNode x            -> SendActivity.id x.node
    | ReceiveNode x         -> ReceiveActivity.id x.node
    | XORGatewayStartNode x -> XORGateway.id x.xor
    | XORGatewayEndNode x   -> XORGateway.id x.xor
    | ANDGatewayStartNode x -> ANDGateway.id x.par
    | ANDGatewayEndNode x   -> ANDGateway.id x.par
    in from_uuid uuid
  (* }}}*)
  let to_string node =(* {{{*)
    match node with
    | StartNode x           -> StartEvent.to_string x.start_node
    | EndNode x             -> EndEvent.to_string x.end_node
    | SendNode x            -> SendActivity.to_string x.node
    | ReceiveNode x         -> ReceiveActivity.to_string x.node
    | XORGatewayStartNode x -> XORGateway.to_string x.xor
    | XORGatewayEndNode x   -> XORGateway.to_string x.xor
    | ANDGatewayStartNode x -> ANDGateway.to_string x.par
    | ANDGatewayEndNode x   -> ANDGateway.to_string x.par
  (* }}}*)
  let of_choreography_node node ~role =(* {{{*)
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
  let rec traverse' node level ~init ~f =(* {{{*)
    match node with
    | StartNode x           -> maybe_traverse_child x.outgoing level (f init level node) f
    | EndNode x             -> f init level node
    | SendNode x            -> maybe_traverse_child x.outgoing level (f init level node) f
    | ReceiveNode x         -> maybe_traverse_child x.outgoing level (f init level node) f
    | XORGatewayStartNode x -> list_traverse_children x.outgoing (level + 1) (f init (level + 1) node) f
    | XORGatewayEndNode x   -> maybe_traverse_child x.outgoing (level - 1) (f init level node) f
    | ANDGatewayStartNode x -> list_traverse_children x.outgoing (level + 1) (f init (level + 1) node) f
    | ANDGatewayEndNode x   -> maybe_traverse_child x.outgoing (level - 1) (f init level node) f

  and maybe_traverse_child node level state f =
    match node with
      | None -> state
      | Some next_node -> traverse' next_node level ~init:state ~f:f

  and list_traverse_children nodes level state f =
    List.fold ~init:state ~f:(fun next_state child ->
        traverse' child level ~init:next_state ~f:f)
      nodes
  (* }}}*)
  let traverse node ~init ~f = traverse' node 1 ~init:init ~f:f
  let add_outgoing node successors =(* {{{*)
    match node with
    | StartNode x -> assert (List.length successors = 1); StartNode { x with outgoing = List.hd successors }
    | EndNode x -> EndNode x
    | SendNode x -> assert (List.length successors = 1); SendNode { x with outgoing = List.hd successors }
    | ReceiveNode x -> assert (List.length successors = 1); ReceiveNode { x with outgoing = List.hd successors }
    | XORGatewayStartNode x -> XORGatewayStartNode { x with outgoing = successors }
    | XORGatewayEndNode x -> assert (List.length successors = 1); XORGatewayEndNode { x with outgoing = List.hd successors }
    | ANDGatewayStartNode x -> ANDGatewayStartNode { x with outgoing = successors }
    | ANDGatewayEndNode x -> assert (List.length successors = 1); ANDGatewayEndNode { x with outgoing = List.hd successors }
    (* }}}*)
  let to_string_deep node =
    traverse node ~init:"" ~f:(fun state level next_node ->
        Printf.sprintf "%s%s%s\n"
          state
          (level_str level)
          (to_string next_node))
end

module PublicNodes : sig
  type t
  val project: Choreography.choreography_node -> role:string -> t
  val start_node: t -> public_node
end = struct
  type t = { nodes: public_node String.Map.t
           ; graph: public_node
           }

  let is_eligable_node chor_node by_role =
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

  let rec find_eligable_successors chor_node by_role =
    match chor_node with
    | Choreography.StartNode x       -> maybe_find_eligable_successor x.outgoing by_role 
    | Choreography.EndNode x         -> []
    | Choreography.InteractionNode x -> maybe_find_eligable_successor x.outgoing by_role
    | Choreography.XORGatewayStartNode x -> collect_eligable_succesors x.outgoing by_role
    | Choreography.XORGatewayEndNode x -> maybe_find_eligable_successor x.outgoing by_role
    | Choreography.ANDGatewayStartNode x -> collect_eligable_succesors x.outgoing by_role
    | Choreography.ANDGatewayEndNode x -> maybe_find_eligable_successor x.outgoing by_role

  and maybe_find_eligable_successor maybe_chor_node by_role =
    match maybe_chor_node with
    | None -> []
    | Some next_node -> if is_eligable_node next_node by_role
      then [(PublicNode.of_choreography_node next_node ~role:by_role)]
      else find_eligable_successors next_node by_role

  and collect_eligable_succesors next_nodes by_role =
    List.fold ~init:[] ~f:(fun state next_node ->
        if is_eligable_node next_node by_role
        then
          let public_node = PublicNode.of_choreography_node next_node ~role:by_role in
          if List.exists state ~f:(fun node -> node = public_node)
          then state
          else public_node :: state
        else find_eligable_successors next_node by_role @ state)
      next_nodes
    |> List.rev

  let update_public_nodes public_nodes chor_node by_role =
    (* build up the public node mapping here, but only if this chor_node is eligable *)
    if is_eligable_node chor_node by_role
    then
      let public_node = match Map.find public_nodes (Choreography.ChoreographyNode.id chor_node) with
        | None   -> PublicNode.of_choreography_node chor_node ~role:by_role
        | Some x -> x
      in
      let successors = find_eligable_successors chor_node by_role in
      let public_node' = PublicNode.add_outgoing public_node successors in
      Map.add public_nodes ~key:(PublicNode.id public_node) ~data:public_node'
    else public_nodes

  let project chor_node ~role =
    let public_nodes, start_node = Choreography.ChoreographyNode.traverse chor_node
        ~init:(String.Map.empty, None)
        ~f:(fun (public_nodes, start_node) level curr_chor_node ->
            let public_nodes' = update_public_nodes public_nodes curr_chor_node role in
            let start_node' = if Choreography.ChoreographyNode.is_start curr_chor_node
              then Some (PublicNode.of_choreography_node curr_chor_node ~role:role)
              else start_node
            in
            (public_nodes', start_node'))
    in
    match start_node with
    | None -> failwith "No Start Node for Public Model projection"
    | Some start_node' ->
      (* TODO: reindex graph nodes from the mapping *)
      print_endline @@ Map.fold ~init:""
        ~f:(fun ~key ~data state ->
            Printf.sprintf "%skey:%s -> node:%s\n"
              state
              key
              (PublicNode.to_string_deep data)) public_nodes;
      { nodes = public_nodes
      ; graph = start_node'
      }

  let start_node self = self.graph

end

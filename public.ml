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

module PublicNode : sig(* {{{ *)
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
  let to_string_deep node =(* {{{*)
    traverse node ~init:"" ~f:(fun state level next_node ->
        Printf.sprintf "%s%s%s\n"
          state
          (level_str (level * 2))
          (to_string next_node))
    (* }}}*)
end
(* }}} *)
module PublicNodeRev : Node with type t = public_node = struct(* {{{ *)
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
    | XORGatewayStartNode x -> `list_successors (x.outgoing,  1, 1)
    | XORGatewayEndNode x   -> `maybe_successor (x.outgoing, -1, 0)
    | ANDGatewayStartNode x -> `list_successors (x.outgoing,  1, 1)
    | ANDGatewayEndNode x   -> `maybe_successor (x.outgoing, -1, 0)
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
end
(* }}} *)

module PublicTraverse = Make_Traversable (PublicNodeRev)
module PublicRPST = Make_RPST (PublicNodeRev) (PublicTraverse)

module PublicNodes : sig(* {{{*)
  type t
  val project: Choreography.choreography_node -> role:string -> t
  val start_node: t -> public_node
end = struct
  type t = { nodes: public_node String.Map.t
           ; graph: public_node
           }

  let maybe_uuid maybe_node =(* {{{*)
    match maybe_node with
    | None -> failwith "non-existent public node for uuid lookup"
    | Some node -> PublicNode.id node
  (* }}}*)
  let rec update_outgoing self start_uuid current_uuid =(* {{{*)
    printf "update_outgoing: UUID=%s" current_uuid;
    let node' = match Map.find self current_uuid with
    | None -> failwith @@ "non-existent node with id: " ^ current_uuid
    | Some node' -> node'
    in
    match node' with
    | StartNode x -> let self', next_key = handle_simple node' x.outgoing self start_uuid in
      let updated_node = StartNode { x with outgoing = Map.find self' next_key } in
      Map.add self' ~key:current_uuid ~data:updated_node, None
    | EndNode x -> self, None
    | SendNode x -> let self', next_key = handle_simple node' x.outgoing self start_uuid in
      let updated_node = SendNode { x with outgoing = Map.find self' next_key } in
      Map.add self' ~key:current_uuid ~data:updated_node, None
    | ReceiveNode x -> let self', next_key = handle_simple node' x.outgoing self start_uuid in
      let updated_node = ReceiveNode { x with outgoing = Map.find self' next_key } in
      Map.add self' ~key:current_uuid ~data:updated_node, None
    | XORGatewayStartNode x -> let self', successors = handle_list node' x.outgoing self start_uuid in
      let updated_node = XORGatewayStartNode { x with outgoing = successors } in
      Map.add self' ~key:current_uuid ~data:updated_node, None
    | XORGatewayEndNode x -> let self', next_key = handle_simple node' x.outgoing self start_uuid in
      let updated_node = XORGatewayEndNode { x with outgoing = Map.find self' next_key } in
      Map.add self' ~key:current_uuid ~data:updated_node, None
    | ANDGatewayStartNode x -> let self', successors = handle_list node' x.outgoing self start_uuid in
      let updated_node = ANDGatewayStartNode { x with outgoing = successors } in
      Map.add self' ~key:current_uuid ~data:updated_node, None
    | ANDGatewayEndNode x -> let self', next_key = handle_simple node' x.outgoing self start_uuid in
      let updated_node = ANDGatewayEndNode { x with outgoing = Map.find self' next_key } in
      let self' = Map.add self' ~key:current_uuid ~data:updated_node in
      self', None
  (* }}}*)
  and handle_simple node maybe_next_node self start_uuid =(* {{{*)
    printf "IN: %s\n" @@ PublicNode.to_string node;
    let next_key = maybe_uuid maybe_next_node in
    printf "NEXT KEY: %s\n" next_key;
    let self', _ = reindex' self start_uuid @@ Some next_key in
    self', next_key
  (* }}}*)
  and handle_list node' next_nodes self start_uuid =(* {{{*)
    printf "IN: %s\n" @@ PublicNode.to_string node';
    let self', successor_uuids = List.fold ~init:(self, [])
        ~f:(fun (state, successor_acc) successor ->
            let next_key = PublicNode.id successor in
            printf "NEXT KEY: %s\n" next_key;
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
      | Some start_node -> PublicNode.id start_node
    in
    if uuid = ""
       then self, None
       else let self', _ = reindex' self uuid (Some uuid)
         in self', Map.find self' uuid
         (* }}}*)

  let is_eligable_node chor_node by_role =(* {{{*)
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
  let rec find_eligable_successors chor_node by_role =(* {{{*)
    match chor_node with
    | Choreography.StartNode x       -> maybe_find_eligable_successor x.outgoing by_role 
    | Choreography.EndNode x         -> []
    | Choreography.InteractionNode x -> maybe_find_eligable_successor x.outgoing by_role
    | Choreography.XORGatewayStartNode x -> collect_eligable_succesors x.outgoing by_role
    | Choreography.XORGatewayEndNode x -> maybe_find_eligable_successor x.outgoing by_role
    | Choreography.ANDGatewayStartNode x -> collect_eligable_succesors x.outgoing by_role
    | Choreography.ANDGatewayEndNode x -> maybe_find_eligable_successor x.outgoing by_role
  (* }}}*)
  and maybe_find_eligable_successor maybe_chor_node by_role =(* {{{*)
    match maybe_chor_node with
    | None -> []
    | Some next_node -> if is_eligable_node next_node by_role
      then [(PublicNode.of_choreography_node next_node ~role:by_role)]
      else find_eligable_successors next_node by_role
  (* }}}*)
  and collect_eligable_succesors next_nodes by_role =(* {{{*)
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
  (* }}}*)
  let update_public_nodes public_nodes chor_node by_role =(* {{{*)
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
  (* }}}*)
  let project chor_node ~role =(* {{{*)
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
    let public_nodes', start_node' = reindex public_nodes start_node in
    print_endline @@ Map.fold ~init:""
      ~f:(fun ~key ~data state ->
          Printf.sprintf "%skey:%s -> node:%s\n"
            state
            key
            (PublicNode.to_string_deep data)) public_nodes;
    match start_node' with
    | None -> failwith "no public start node found"
    | Some start_node' ->
      print_endline "THE WHOLE PUBLIC MODEL:";
      print_endline @@ PublicNode.to_string_deep start_node';
      { nodes = public_nodes'
      ; graph = start_node'
      }
  (* }}}*)
  let start_node self = self.graph

end
(* }}}*)

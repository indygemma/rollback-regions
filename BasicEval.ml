open Core.Std

(* TODO: message, participant, start event and end event all only have id and name. refactor to signature *)
module Message : sig(* {{{*)
  type t
  val create : name:string -> id:Uuid.t -> t
  val name : t -> string
  val id : t -> Uuid.t
  val to_string: t -> string
end = struct
  type t = { name: string
           ; id: Uuid.t
           }
  let create ~name ~id = { name; id }
  let name self = self.name
  let id self = self.id
  let to_string self = self.name
end(* }}}*)
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
module MessageFlow : sig(* {{{*)
  type t
  val create : sender:Participant.t -> receiver:Participant.t -> message:Message.t option -> t
  val sender : t -> Participant.t
  val receiver : t -> Participant.t
  val message : t -> Message.t option
end = struct
  type t = { sender : Participant.t
           ; receiver : Participant.t
           ; message : Message.t option
           }
  let create ~sender ~receiver ~message = { sender; receiver; message }
  let sender self = self.sender
  let receiver self = self.receiver
  let message self = self.message
end(* }}}*)
module Interaction : sig(* {{{*)
  type t
  val create : id:Uuid.t -> message_flow:MessageFlow.t -> name:string -> t
  val sender : t -> Participant.t
  val receiver : t -> Participant.t
  val message : t -> Message.t option
  val name : t -> string
  val to_string: t -> string
  val id: t -> Uuid.t
end = struct
  type t = { id: Uuid.t
           ; sender: Participant.t
           ; receiver: Participant.t
           ; message: Message.t option
           ; name: string
           }
  let create ~id ~message_flow ~name = { id
                                       ; sender = MessageFlow.sender message_flow
                                       ; receiver = MessageFlow.receiver message_flow
                                       ; message = MessageFlow.message message_flow
                                       ; name
                                       }
  let sender self = self.sender
  let receiver self = self.receiver
  let message self = self.message
  let name self = self.name
  let id self = self.id
  let to_string self =
    let msg = match self.message with
      | None -> "(no message)"
      | Some msg' -> Message.to_string msg'
    in
    Printf.sprintf "(%s) Interaction: %s. Sender: %s -> Receiver: %s. Message: %s"
      (Uuid.to_string self.id)
      self.name
      (Participant.to_string self.sender)
      (Participant.to_string self.receiver)
      msg
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

type node = StartNode of { start_node: StartEvent.t ; outgoing: node option }
          | EndNode of { end_node: EndEvent.t }
          | InteractionNode of { interaction: Interaction.t ; outgoing: node option }
          | XORGatewayStartNode of { xor: XORGateway.t ; outgoing: node list }
          | XORGatewayEndNode of { xor: XORGateway.t ; outgoing: node option }
          | ANDGatewayStartNode of { par: ANDGateway.t ; outgoing: node list }
          | ANDGatewayEndNode of { par: ANDGateway.t ; outgoing: node option }

module Node : sig
  val add_outgoing: node -> node -> node
  val empty_start: node option
  val choose_start: node -> node option -> node option
  val to_string: int -> node -> string
  val maybe_to_string: int -> node option -> string
  val id: node -> string
end = struct
  let add_outgoing target source =
    match source with
    | StartNode x           -> StartNode { x with outgoing = Some target }
    | EndNode x             -> EndNode x
    | InteractionNode x     -> InteractionNode { x with outgoing = Some target }
    | XORGatewayStartNode x -> XORGatewayStartNode { x with outgoing = target :: x.outgoing }
    | XORGatewayEndNode x   -> XORGatewayEndNode { x with outgoing = Some target }
    | ANDGatewayStartNode x -> ANDGatewayStartNode { x with outgoing = target :: x.outgoing }
    | ANDGatewayEndNode x   -> ANDGatewayEndNode { x with outgoing = Some target }

  let empty_start = None
  let is_start node = match node with
    | StartNode _ -> true
    | _ -> false
  let choose_start left maybe_right =
    match (is_start left, maybe_right) with
    | (false, None      ) -> None
    | (false, Some right) -> if is_start right then Some right else None
    | (true, _          ) -> Some left


  let level_str level = List.range 0 level |> List.fold ~init:"" ~f:(fun state x -> state ^ " ")

  let rec to_string level node =
    match node with
    | StartNode x -> Printf.sprintf "%s%s%s"
                       (level_str level)
                       (StartEvent.to_string x.start_node)
                       ("\n" ^ maybe_to_string (level + 4) x.outgoing)
    | EndNode x -> Printf.sprintf "%s%s"
                     (level_str @@ level - 4)
                     (EndEvent.to_string x.end_node)
    | InteractionNode x -> Printf.sprintf "%s%s%s"
                             (level_str level)
                             (Interaction.to_string x.interaction)
                             ("\n" ^ maybe_to_string level x.outgoing)
    | XORGatewayStartNode x -> Printf.sprintf "%s%s%s"
                                 (level_str level)
                                 (XORGateway.to_string x.xor)
                                 ("\n" ^ list_to_string (level + 4) x.outgoing)
    | XORGatewayEndNode x -> Printf.sprintf "%s%s%s"
                               (level_str @@ level - 4)
                               (XORGateway.to_string x.xor)
                               ("\n" ^ maybe_to_string (level - 4) x.outgoing)
    | ANDGatewayStartNode x -> Printf.sprintf "%s%s%s"
                                 (level_str level)
                                 (ANDGateway.to_string x.par)
                                 ("\n" ^ list_to_string (level + 4) x.outgoing)
    | ANDGatewayEndNode x -> Printf.sprintf "%s%s%s"
                               (level_str @@ level - 4)
                               (ANDGateway.to_string x.par)
                               ("\n" ^ maybe_to_string (level - 4) x.outgoing)

  and maybe_to_string level node =
    match node with
    | None -> ""
    | Some node' -> to_string level node'

  and list_to_string level node_list =
    List.fold ~init:"" ~f:(fun state node ->
        state ^ "\n" ^ (to_string level node))
      node_list

  let id node =
    match node with
    | StartNode x -> StartEvent.id x.start_node |> from_uuid
    | EndNode x -> EndEvent.id x.end_node |> from_uuid
    | InteractionNode x -> Interaction.id x.interaction |> from_uuid
    | XORGatewayStartNode x -> XORGateway.id x.xor |> from_uuid
    | XORGatewayEndNode x -> XORGateway.id x.xor |> from_uuid
    | ANDGatewayStartNode x -> ANDGateway.id x.par |> from_uuid
    | ANDGatewayEndNode x -> ANDGateway.id x.par |> from_uuid

end

module Nodes : sig
  type t
  val lookup: uuid:string -> StartEvent.t String.Map.t -> EndEvent.t String.Map.t -> Interaction.t String.Map.t -> XORGateway.t String.Map.t -> ANDGateway.t String.Map.t -> t -> node
  val add: uuid:string -> node -> t -> t
  val empty: t
  val reindex: t -> node option -> t * node option
end = struct
  type t = node String.Map.t

  let lookup ~uuid start_events end_events choreography_tasks xor_gateways and_gateways self =
    match (Map.find start_events uuid,
           Map.find end_events uuid,
           Map.find choreography_tasks uuid,
           Map.find xor_gateways uuid,
           Map.find and_gateways uuid,
           Map.find self uuid) with
    | (_, _, _, _, _, Some node)                 -> print_endline "lookup from existing nodes"; node
    | (Some start_event, _, _, _, _, _)          -> StartNode { start_node = start_event ; outgoing = None }
    | (None, Some end_event, _, _, _, _)         -> EndNode { end_node = end_event }
    | (None, None, Some interaction, _, _, _)    -> print_endline "lookup from new interaction"; InteractionNode { interaction = interaction; outgoing = None }
    | (None, None, None, Some xor_gateway, _, _) -> 
      if XORGateway.is_start xor_gateway
      then XORGatewayStartNode { xor = xor_gateway; outgoing = [] }
      else XORGatewayEndNode { xor = xor_gateway; outgoing = None }
    | (None, None, None, None, Some and_gateway, _) ->
      if ANDGateway.is_start and_gateway
      then ANDGatewayStartNode { par = and_gateway; outgoing = [] }
      else ANDGatewayEndNode { par = and_gateway; outgoing = None }
    | (None, None, None, None, None, None) -> failwith @@ "Non-existent node: " ^ uuid

  let add ~uuid target self =
    printf "adding key: %s -> %s\n" uuid (Node.to_string 0 target);
    Map.add self ~key:uuid ~data:target

  let empty = String.Map.empty

  let maybe_uuid maybe_node =
    match maybe_node with
    | None -> failwith "non-existent node for uuid lookup"
    | Some node -> Node.id node

  let rec update_outgoing self start_uuid current_uuid =
    printf "update_outgoing: UUID=%s" current_uuid;
    let node = Map.find self current_uuid in
    let node' = match node with
    | None -> failwith @@ "non-existent node with id: " ^ current_uuid
    | Some node' -> node'
    in
    match node' with
    | StartNode x ->
      printf "IN: %s\n" @@ StartEvent.to_string x.start_node;
      let next_key = maybe_uuid x.outgoing in
      printf "NEXT KEY: %s\n" next_key;
      let self', _ = reindex' self start_uuid @@ Some next_key in
      let updated_node = StartNode { x with outgoing = Map.find self' next_key } in
      let self' = Map.add self' ~key:current_uuid ~data:updated_node in
      self', None
    | EndNode x -> self, None
    | InteractionNode x ->
      printf "IN: %s\n" @@ Interaction.to_string x.interaction;
      let next_key = maybe_uuid x.outgoing in
      printf "NEXT KEY: %s\n" next_key;
      let self', _ = reindex' self start_uuid @@ Some next_key in
      let updated_node = InteractionNode { x with outgoing = Map.find self' next_key } in
      let self' = Map.add self' ~key:current_uuid ~data:updated_node in
      self', None
    | XORGatewayStartNode x ->
      printf "IN: %s\n" @@ XORGateway.to_string x.xor;
      let self', successor_uuids = List.fold ~init:(self, [])
          ~f:(fun (state, successor_acc) successor ->
              let next_key = Node.id successor in
              printf "NEXT KEY: %s\n" next_key;
              let state', _ = reindex' state start_uuid @@ Some next_key in
              state', next_key :: successor_acc
            ) x.outgoing in
      let successors' = List.fold ~init:[] ~f:(fun state uuid ->
          match Map.find self' uuid with
          | None -> state
          | Some x -> x :: state)
          successor_uuids
      in
      let updated_node = XORGatewayStartNode { x with outgoing = successors' } in
      let self' = Map.add self' ~key:current_uuid ~data:updated_node in
      self', None
    | XORGatewayEndNode x ->
      printf "IN: %s\n" @@ XORGateway.to_string x.xor;
      let next_key = maybe_uuid x.outgoing in
      printf "NEXT KEY: %s\n" next_key;
      let self', _ = reindex' self start_uuid @@ Some next_key in
      let updated_node = XORGatewayEndNode { x with outgoing = Map.find self' next_key } in
      let self' = Map.add self' ~key:current_uuid ~data:updated_node in
      self', None
    | ANDGatewayStartNode x ->
      printf "IN: %s\n" @@ ANDGateway.to_string x.par;
      printf "before: %s\n" (Node.to_string 0 node');
      let self', successor_uuids = List.fold ~init:(self, [])
          ~f:(fun (state, successor_acc) successor ->
              let next_key = Node.id successor in
              printf "NEXT KEY: %s\n" next_key;
              let state', _ = reindex' state start_uuid @@ Some next_key in
              state', next_key :: successor_acc
            ) x.outgoing in
      let successors' = List.fold ~init:[] ~f:(fun state uuid ->
          match Map.find self' uuid with
          | None -> state
          | Some x -> x :: state)
          successor_uuids
      in
      let updated_node = ANDGatewayStartNode { x with outgoing = successors' } in
      printf "after: %s\n" (Node.to_string 0 updated_node);
      let self' = Map.add self' ~key:current_uuid ~data:updated_node in
      self', None
    | ANDGatewayEndNode x ->
      printf "IN: %s\n" @@ ANDGateway.to_string x.par;
      let next_key = maybe_uuid x.outgoing in
      printf "NEXT KEY: %s\n" next_key;
      let self', _ = reindex' self start_uuid @@ Some next_key in
      let updated_node = ANDGatewayEndNode { x with outgoing = Map.find self' next_key } in
      let self' = Map.add self' ~key:current_uuid ~data:updated_node in
      self', None

  and reindex' self start_uuid maybe_current_uuid =
    match maybe_current_uuid with
    | None -> (self, Map.find self start_uuid)
    | Some current_uuid -> update_outgoing self start_uuid current_uuid

  let reindex self maybe_start_node =

    (* the nodes map is at this point complete, need to reindex all the
       outgoing nodes to the fully mapped ones. Use the uuids to look the
       correct nodes *)

    let uuid = match maybe_start_node with
      | None -> ""
      | Some start_node -> Node.id start_node
    in
    if uuid = ""
       then self, None
       else let self', _ = reindex' self uuid (Some uuid)
         in let result = self', Map.find self' uuid in
         print_endline "lookup specific -----------------";
         let _ = match Map.find self' "sid-B2B2EF88-2BFE-4C24-A30A-5898A63EB19A" with
           | None -> print_endline "nothing..."
           | Some node -> print_endline @@ Node.to_string 0 node in
         print_endline "lookup specific END -------------";
         result

end

module BPMNParser : sig
  type t
  val parse : filename:string -> t
  val to_string: t -> string
end = struct
  type t = { filename : string
           ; messages : Message.t String.Map.t
           ; participants : Participant.t String.Map.t
           ; message_flows : MessageFlow.t String.Map.t
           ; start_events : StartEvent.t String.Map.t
           ; end_events : EndEvent.t String.Map.t
           ; choreography_tasks : Interaction.t String.Map.t
           ; xor_gateways : XORGateway.t String.Map.t
           ; and_gateways : ANDGateway.t String.Map.t
           ; nodes : Nodes.t
           ; graph : node option
           ; soup : Soup.soup Soup.node
           }

  let parse_participants soup =(* {{{*)
    print_endline "---- Participants: ----";
    let open Soup in
    soup $$ "participant" |> fold (fun participants' node ->
        let id = node |> R.attribute "id" in
        let name = node |> R.attribute "name" in
        printf "%s: %s\n" id name;
        Map.add participants' ~key:id ~data:(Participant.create ~id:(to_uuid id) ~name:name)
      ) String.Map.empty
  (* }}}*)
  let parse_messages soup =(* {{{*)
    print_endline "---- Messages: ----";
    let open Soup in
    soup $$ "message" |> fold (fun messages node ->
        let id = node |> R.attribute "id" in
        let name = node |> R.attribute "name" in
        printf "%s: %s\n" id name;
        Map.add messages ~key:id ~data:(Message.create ~id:(to_uuid id) ~name:name)
      ) String.Map.empty
  (* }}}*)
  let parse_message_flows soup participants messages =(* {{{*)
    print_endline "---- Message Flow (interactions): ----";
    let open Soup in
    soup $$ "messageFlow" |> fold (fun message_flows node ->
        let id = node |> R.attribute "id" in
        let has_message = has_attribute "messageRef" node in
        let maybe_message, messageRef = if has_message
          then let messageRef = node |> R.attribute "messageRef" in
               (Map.find messages messageRef, messageRef)
          else (None, "") in
        print_endline @@ "messageRef: " ^ messageRef;
        print_endline @@ Soup.to_string node;
        let sourceRef = node |> R.attribute "sourceRef" in
        print_endline @@ "sourceRef: " ^ sourceRef;
        let targetRef = node |> R.attribute "targetRef" in
        print_endline @@ "targetRef: " ^ targetRef;
        match (has_message, maybe_message, Map.find participants sourceRef, Map.find participants targetRef) with
        | (true, None, _,    _   ) -> failwith @@ "non-existent message id: " ^ messageRef
        | (_   , _,    None, _   ) -> failwith @@ "non-existent participant: " ^ sourceRef
        | (_,    _,    _,    None) -> failwith @@ "non-existent participant: " ^ targetRef
        | (true, Some message, Some source, Some target) ->
          printf "adding interaction: %s->%s message: %s\n" (Participant.name source) (Participant.name target) (Message.name message);
          Map.add message_flows ~key:id ~data:(MessageFlow.create ~sender:source ~receiver:target ~message:(Some message))
        | (false, _          , Some source, Some target) ->
          printf "adding interaction: %s->%s without message\n" (Participant.name source) (Participant.name target);
          Map.add message_flows ~key:id ~data:(MessageFlow.create ~sender:source ~receiver:target ~message:None)
      ) String.Map.empty
  (* }}}*)
  let parse_start_events soup =(* {{{*)
    (* <startEvent id="" name=""> <outgoing>id</outgoing> -> outgoing id should be a <choreographyTask/> *)
    let open Soup in
    soup $$ "startEvent" |> fold (fun start_events node ->
        let id = node |> R.attribute "id" in
        let name = node |> R.attribute "name" in
        printf "adding start event: %s %s\n" name id;
        Map.add start_events ~key:id ~data:(StartEvent.create ~id:(to_uuid id) ~name:name)
      ) String.Map.empty
  (* }}}*)
  let parse_end_events soup =(* {{{*)
    (* <endEvent id="" name=""> <incoming>id</incoming> -> *)
    let open Soup in
    soup $$ "endEvent" |> fold (fun end_events node ->
        let id = node |> R.attribute "id" in
        let name = node |> R.attribute "name" in
        printf "adding end event: %s %s\n" name id;
        Map.add end_events ~key:id ~data:(EndEvent.create ~id:(to_uuid id) ~name:name)
      ) String.Map.empty
  (* }}}*)
  let parse_choreography_tasks soup message_flows =(* {{{*)
    (* <choreographyTask id="" initiatingParticipantRef="" loopType="None" name="">
       <incoming>id</incoming>
       <outgoing>id</outgoing>
       <messageFlowRef>id</messageFlowRef>
    *)
    let open Soup in
    soup $$ "choreographyTask" |> fold (fun choreography_tasks node ->
        let id = node |> R.attribute "id" in
        print_endline @@ "id: " ^ id;
        let name = node |> R.attribute "name" in
        print_endline @@ "name: " ^ name;
        let messageFlowRef = node $$ "messageFlowRef"
                             |> fold (fun result node -> R.leaf_text node) "" in
        assert (messageFlowRef <> "");
        printf "choreographyTask: id=%s name=%s messageFlowRef=%s\n" id name messageFlowRef;
        match (Map.find message_flows messageFlowRef) with
        | None -> failwith @@ "non-existent message flow: " ^ messageFlowRef
        | Some message_flow ->
          Map.add choreography_tasks ~key:id ~data:(Interaction.create ~id:(to_uuid id) ~message_flow:message_flow ~name:name)
      ) String.Map.empty
  (* }}}*)
  let parse_xor_gateways soup =(* {{{*)
    (* exclusiveGateway gatewayDirection="Diverging|Converging" id="" name=""

       <incoming>...
       <outgoing>...
       <outgoing>...
    *)
    let open Soup in
    soup $$ "exclusiveGateway" |> fold (fun result node ->
        let id = node |> R.attribute "id" in
        let name = node |> R.attribute "name" in
        let gateway_direction = node |> R.attribute "gatewayDirection" in
        let gateway_type = if gateway_direction = "Diverging" then `Start else `End in
        printf "XOR Gateway %s: %s (%s)\n" id name gateway_direction;
        Map.add result ~key:id ~data:(XORGateway.create ~id:(to_uuid id) ~name:name gateway_type)
      ) String.Map.empty
  (* }}}*)
  let parse_and_gateways soup =(* {{{*)
    (* parallelGateway gatewayDirection="Diverging|Converging" id="" name=""
       <incoming>...
       <outgoing>...
       <outgoing>...
    *)
    let open Soup in
    soup $$ "parallelGateway" |> fold (fun result node ->
        let id = node |> R.attribute "id" in
        let name = node |> R.attribute "name" in
        let gateway_direction = node |> R.attribute "gatewayDirection" in
        let gateway_type = if gateway_direction = "Diverging" then `Start else `End in
        printf "AND Gateway %s: %s (%s)\n" id name gateway_direction;
        Map.add result ~key:id ~data:(ANDGateway.create ~id:(to_uuid id) ~name:name gateway_type)
      ) String.Map.empty
  (* }}}*)
  let build_graph soup start_events end_events choreography_tasks xor_gateways and_gateways =(* {{{*)
    (* sequenceFlow id="" isImmediate="true" sourceRef="" targetRef=""
       This is the flow that connects all the nodes together from source to target (start element, gateways, choreographyTask, end element)
       Use this element to build up the graph. First create maps for each startEvent, end element gateways and choreography tasks.
       From the individual nodes I only would need the name.
    *)
    let open Soup in
    let nodes, start_node = soup $$ "sequenceFlow" |> fold (fun (nodes, start_node) node ->
        let source_ref = node |> R.attribute "sourceRef" in
        let target_ref = node |> R.attribute "targetRef" in
        printf "sequence flow: %s (%s)\n" source_ref target_ref;
        let target_node = Nodes.lookup ~uuid:target_ref start_events end_events choreography_tasks xor_gateways and_gateways nodes in
        let source_node = Nodes.lookup ~uuid:source_ref start_events end_events choreography_tasks xor_gateways and_gateways nodes
                          |> Node.add_outgoing target_node in
        let nodes' = Nodes.add ~uuid:target_ref target_node nodes
                     |> Nodes.add ~uuid:source_ref source_node in
        printf "source: %s\n" (Node.to_string 0 source_node);
        printf "target: %s\n" (Node.to_string 0 target_node);
        printf "start node: %s\n" @@ Node.maybe_to_string 0 @@ Node.choose_start source_node start_node;
        (nodes', Node.choose_start source_node start_node)
      ) (Nodes.empty, Node.empty_start)
    in
    let _ = match start_node with
    | None -> print_endline "empty start node? why";
    | Some start_node' -> print_endline @@ "there is a start node: " ^ (Node.to_string 0 start_node')
    in
    Nodes.reindex nodes start_node
  (* }}}*)
  let parse ~filename =(* {{{*)
    let open Soup in
    let soup = read_file filename |> Markup.string |> Markup.parse_xml |> Markup.signals |> from_signals in
    soup $$ "choreography" |> iter (fun a -> a |> R.attribute "id" |> print_endline);
    let participants       = parse_participants soup in
    let messages           = parse_messages soup in
    let message_flows      = parse_message_flows soup participants messages in
    let start_events       = parse_start_events soup in
    let end_events         = parse_end_events soup in
    let choreography_tasks = parse_choreography_tasks soup message_flows in
    let xor_gateways       = parse_xor_gateways soup in
    let and_gateways       = parse_and_gateways soup in
    let nodes, graph       = build_graph soup start_events end_events choreography_tasks xor_gateways and_gateways in
    { filename = filename
    ; messages = messages
    ; participants = participants
    ; message_flows = message_flows
    ; start_events = start_events
    ; end_events = end_events
    ; choreography_tasks = choreography_tasks
    ; xor_gateways = xor_gateways
    ; and_gateways = and_gateways
    ; nodes = nodes
    ; graph = graph
    ; soup = soup
    }
    (* }}}*)
  let to_string self =(* {{{*)
    match self.graph with
    | None -> "(empty graph)"
    | Some start -> Node.to_string 0 start
    (* }}}*)

end

let test () =
  let parsed = BPMNParser.parse ~filename:"BookTripOperation.xml" in
  print_endline "======";
  print_endline @@ BPMNParser.to_string parsed;
  (*Uuid.create ()*)
  (*|> Uuid.sexp_of_t*)
  (*|> Sexp.to_string*)
  (*|> print_endline;*)
  (*print_endline "test";*)

  (*Sexp.of_string "f32f66f9-f2dd-3378-3318-ea4b748e3be4"*)
  (*|> Uuid.t_of_sexp*)
  (*|> Uuid.to_string*)
  (*|> print_endline;*)

  ()

let () = test ()

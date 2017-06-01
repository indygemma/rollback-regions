open Core.Std
open Common

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

type choreography_node =
    StartNode              of { start_node: StartEvent.t ; outgoing: choreography_node option }
  | EndNode                of { end_node: EndEvent.t }
  | InteractionNode        of { interaction: Interaction.t ; outgoing: choreography_node option }
  | XORGatewayStartNode    of { xor: XORGateway.t ; outgoing: choreography_node list }
  | XORGatewayEndNode      of { xor: XORGateway.t ; outgoing: choreography_node option }
  | ANDGatewayStartNode    of { par: ANDGateway.t ; outgoing: choreography_node list }
  | ANDGatewayEndNode      of { par: ANDGateway.t ; outgoing: choreography_node option }

module ChoreographyNode : Node with type t = choreography_node = struct(* {{{ *)
  type t = choreography_node

  let rpst_class = function(* {{{ *)
    | StartNode _           -> `start_node
    | XORGatewayStartNode _ -> `xor_node
    | ANDGatewayStartNode _ -> `and_node
    | _                     -> `rest
  (* }}} *)
  let rpst_fragment_class = function(* {{{ *)
    | StartNode _           -> `add_fragment
    | EndNode _             -> `close_start_node
    | InteractionNode _     -> `none
    | XORGatewayStartNode _ -> `add_fragment
    | XORGatewayEndNode _   -> `close_xor_node
    | ANDGatewayStartNode _ -> `add_fragment
    | ANDGatewayEndNode _   -> `close_and_node
  (* }}} *)
  let traverse_class = function(* {{{ *)
    | StartNode x           -> `maybe_successor (x.outgoing,  0, 0)
    | EndNode x             -> `stop
    | InteractionNode x     -> `maybe_successor (x.outgoing,  0, 0)
    | XORGatewayStartNode x -> `list_successors (x.outgoing,  1, 1)
    | XORGatewayEndNode x   -> `maybe_successor (x.outgoing, -1, 0)
    | ANDGatewayStartNode x -> `list_successors (x.outgoing,  1, 1)
    | ANDGatewayEndNode x   -> `maybe_successor (x.outgoing, -1, 0)
(* }}} *)
  let update_node_map = function(* {{{ *)
    | StartNode x -> `update_single x.outgoing
    | EndNode x -> `no_update
    | InteractionNode x -> `update_single x.outgoing
    | XORGatewayStartNode x -> `update_multi x.outgoing
    | XORGatewayEndNode x -> `update_single x.outgoing
    | ANDGatewayStartNode x -> `update_multi x.outgoing
    | ANDGatewayEndNode x -> `update_single x.outgoing
  (* }}} *)

  let id node =(* {{{*)
    match node with
    | StartNode x           -> StartEvent.id x.start_node   |> from_uuid
    | EndNode x             -> EndEvent.id x.end_node       |> from_uuid
    | InteractionNode x     -> Interaction.id x.interaction |> from_uuid
    | XORGatewayStartNode x -> XORGateway.id x.xor          |> from_uuid
    | XORGatewayEndNode x   -> XORGateway.id x.xor          |> from_uuid
    | ANDGatewayStartNode x -> ANDGateway.id x.par          |> from_uuid
    | ANDGatewayEndNode x   -> ANDGateway.id x.par          |> from_uuid
    (* }}}*)
  let to_string node =(* {{{*)
    match node with
    | StartNode x           -> (StartEvent.to_string x.start_node)
    | EndNode x             -> (EndEvent.to_string x.end_node)
    | InteractionNode x     -> (Interaction.to_string x.interaction)
    | XORGatewayStartNode x -> (XORGateway.to_string x.xor)
    | XORGatewayEndNode x   -> (XORGateway.to_string x.xor)
    | ANDGatewayStartNode x -> (ANDGateway.to_string x.par)
    | ANDGatewayEndNode x   -> (ANDGateway.to_string x.par)
  (* }}}*)

  let add_outgoing target source =(* {{{*)
    match source with
    | StartNode x           -> StartNode { x with outgoing = List.hd target }
    | EndNode x             -> EndNode x
    | InteractionNode x     -> InteractionNode { x with outgoing = List.hd target }
    | XORGatewayStartNode x -> XORGatewayStartNode { x with outgoing = merge_unique x.outgoing target }
    | XORGatewayEndNode x   -> XORGatewayEndNode { x with outgoing = List.hd target }
    | ANDGatewayStartNode x -> ANDGatewayStartNode { x with outgoing = merge_unique x.outgoing target }
    | ANDGatewayEndNode x   -> ANDGatewayEndNode { x with outgoing = List.hd target }
  (* }}}*)
  let update_outgoing target source =(* {{{*)
    match source with
    | StartNode x           -> StartNode { x with outgoing = List.hd target }
    | EndNode x             -> EndNode x
    | InteractionNode x     -> InteractionNode { x with outgoing = List.hd target }
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
module ChoreographyTraverse = Make_Traversable (ChoreographyNode)
module ChoreographyRPST = Make_RPST (ChoreographyNode) (ChoreographyTraverse)
module ChoreographyNodeMap = Make_NodeMap (ChoreographyNode)

module BPMNParser : sig(* {{{*)
  type t
  val parse : filename:string -> t
  val to_string: t -> string
  val graph: t -> choreography_node
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
           ; nodes : ChoreographyNodeMap.t
           ; graph : choreography_node option
           ; soup : Soup.soup Soup.node
           }

  let lookup ~uuid start_events end_events choreography_tasks xor_gateways and_gateways self =(* {{{*)
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
  (* }}}*)
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
        let target_node = lookup ~uuid:target_ref start_events end_events choreography_tasks xor_gateways and_gateways nodes in
        let source_node = lookup ~uuid:source_ref start_events end_events choreography_tasks xor_gateways and_gateways nodes
                          |> ChoreographyNode.add_outgoing [target_node] in
        let nodes' = ChoreographyNodeMap.add ~uuid:target_ref target_node nodes
                     |> ChoreographyNodeMap.add ~uuid:source_ref source_node in
        printf "source: %s\n" (ChoreographyTraverse.to_string source_node);
        printf "target: %s\n" (ChoreographyTraverse.to_string target_node);
        let _ = match ChoreographyNode.choose_start source_node start_node with
          | None -> printf "no start\n"
          | Some x -> printf "%s\n" @@ ChoreographyNode.to_string x
        in
        (nodes', ChoreographyNode.choose_start source_node start_node)
      ) (ChoreographyNodeMap.empty, ChoreographyNode.empty_start)
    in
    let _ = match start_node with
    | None -> print_endline "empty start node? why";
    | Some start_node' -> print_endline @@ "there is a start node: " ^ (ChoreographyTraverse.to_string start_node')
    in
    ChoreographyNodeMap.reindex nodes start_node
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
    | Some start -> ChoreographyTraverse.to_string start
    (* }}}*)

  let graph self = match self.graph with
    | None -> failwith "No start node"
    | Some node -> node

end
(* }}}*)

open Core.Std

(* TODO: message, participant, start event and end event all only have id and name. refactor to signature *)
module Message : sig(* {{{*)
  type t
  val create : name:string -> id:Uuid.t -> t
  val name : t -> string
  val id : t -> Uuid.t
end = struct
  type t = { name: string
           ; id: Uuid.t
           }
  let create ~name ~id = { name; id }
  let name self = self.name
  let id self = self.id
end(* }}}*)
module Participant : sig(* {{{*)
  type t
  val create : name:string -> id:Uuid.t -> t
  val name : t -> string
  val id : t -> Uuid.t
end = struct
  type t = { name : string
           ; id : Uuid.t
           }
  let create ~name ~id = { name; id }
  let name self = self.name
  let id self = self.id
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
end(* }}}*)

module StartEvent : sig(* {{{*)
  type t
  val create: id:Uuid.t -> name:string -> t
  val id: t -> Uuid.t
  val name: t -> string
end = struct
  type t = { id: Uuid.t
           ; name: string
           }
  let create ~id ~name = { id; name }
  let id self = self.id
  let name self = self.name
end(* }}}*)
module EndEvent : sig(* {{{*)
  type t
  val create: id:Uuid.t -> name:string -> t
  val id: t -> Uuid.t
  val name: t -> string
end = struct
  type t = { id: Uuid.t
           ; name: string
           }
  let create ~id ~name = { id; name }
  let id self = self.id
  let name self = self.name
end(* }}}*)
module XORGateway : sig(* {{{*)
  type t
  val create: id:Uuid.t -> name:string -> [`Start | `End] -> t
  val id: t -> Uuid.t
  val name: t -> string
  val is_start: t -> bool
  val is_end: t -> bool
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
end(* }}}*)
module ANDGateway : sig(* {{{*)
  type t
  val create: id:Uuid.t -> name:string -> [`Start | `End] -> t
  val id: t -> Uuid.t
  val name: t -> string
  val is_start: t -> bool
  val is_end: t -> bool
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
end(* }}}*)

type node = StartNode of { start_node: StartEvent.t ; outgoing: node }
          | EndNode of { end_node: EndEvent.t }
          | InteractionNode of { interaction: Interaction.t ; outgoing: node }
          | XORGatewayStartNode of { xor: XORGateway.t ; outgoing: node list }
          | XORGatewayEndNode of { xor: XORGateway.t ; outgoing: node }
          | ANDGatewayStartNode of { par: ANDGateway.t ; outgoing: node list }
          | ANDGatewayEndNode of { par: ANDGateway.t ; outgoing: node }

let to_uuid (id : string) = (String.drop_prefix id 4 |> Uuid.of_string)

module BPMNParser : sig
  type t
  val parse : filename:string -> t
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
           ; nodes : node String.Map.t
           ; graph : node option
           ; soup : Soup.soup Soup.node
           }

  let parse_participants soup = 
    print_endline "---- Participants: ----";
    let open Soup in
    soup $$ "participant" |> fold (fun participants' node ->
        let id = node |> R.attribute "id" in
        let name = node |> R.attribute "name" in
        printf "%s: %s\n" id name;
        Map.add participants' ~key:id ~data:(Participant.create ~id:(to_uuid id) ~name:name)
      ) String.Map.empty

  let parse_messages soup =
    print_endline "---- Messages: ----";
    let open Soup in
    soup $$ "message" |> fold (fun messages node ->
        let id = node |> R.attribute "id" in
        let name = node |> R.attribute "name" in
        printf "%s: %s\n" id name;
        Map.add messages ~key:id ~data:(Message.create ~id:(to_uuid id) ~name:name)
      ) String.Map.empty

  let parse_message_flows soup participants messages =
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

  let parse_start_events soup =
    (* <startEvent id="" name=""> <outgoing>id</outgoing> -> outgoing id should be a <choreographyTask/> *)
    let open Soup in
    soup $$ "startEvent" |> fold (fun start_events node ->
        let id = node |> R.attribute "id" in
        let name = node |> R.attribute "name" in
        printf "adding start event: %s %s\n" name id;
        Map.add start_events ~key:id ~data:(StartEvent.create ~id:(to_uuid id) ~name:name)
      ) String.Map.empty

  let parse_end_events soup =
    (* <endEvent id="" name=""> <incoming>id</incoming> -> *)
    let open Soup in
    soup $$ "endEvent" |> fold (fun end_events node ->
        let id = node |> R.attribute "id" in
        let name = node |> R.attribute "name" in
        printf "adding end event: %s %s\n" name id;
        Map.add end_events ~key:id ~data:(EndEvent.create ~id:(to_uuid id) ~name:name)
      ) String.Map.empty

  let parse_choreography_tasks soup message_flows =
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

  let parse_xor_gateways soup =
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

  let parse_and_gateways soup =
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

  let build_graph soup start_events end_events choreography_tasks xor_gateways and_gateways =
    (* sequenceFlow id="" isImmediate="true" sourceRef="" targetRef=""
       This is the flow that connects all the nodes together from source to target (start element, gateways, choreographyTask, end element)
       Use this element to build up the graph. First create maps for each startEvent, end element gateways and choreography tasks.
       From the individual nodes I only would need the name.
    *)
    let open Soup in
    soup $$ "sequenceFlow" |> fold (fun (nodes, start_node) node ->
        let source_ref = node |> R.attribute "sourceRef" in
        let target_ref = node |> R.attribute "targetRef" in
        printf "sequence flow: %s (%s)\n" source_ref target_ref;
        (nodes, start_node)
      ) (String.Map.empty, None)

  let parse ~filename =
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
end

let test () =
  let _ = BPMNParser.parse ~filename:"BookTripOperation.xml" in
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

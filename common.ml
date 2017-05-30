open Core.Std

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

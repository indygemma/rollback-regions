open Core.Std
open Common
open Choreography

let test () =
  let parsed = BPMNParser.parse ~filename:"BookTripOperation.xml" in
  print_endline "======";
  print_endline @@ BPMNParser.to_string parsed;
  let rpst = ChoreographyRPST.calculate (BPMNParser.graph parsed) in
  print_endline @@ ChoreographyRPST.to_string rpst;
  let projection_traveler = Public.PublicNodes.project (BPMNParser.graph parsed) ~role:"Traveler"
  in Public.PublicNodes.start_node projection_traveler |> Public.PublicNode.to_string |> print_endline;
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

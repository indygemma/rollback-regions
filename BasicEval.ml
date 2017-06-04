open Core
open Common
open Choreography
open OUnit2

let test test_ctxt =
  let parsed = BPMNParser.parse ~filename:"BookTripOperation.xml" in
  print_endline "======";
  print_endline @@ BPMNParser.to_string parsed;
  let chor_rpst = ChoreographyRPST.calculate (BPMNParser.graph parsed) in
  printf "RPST (new module): %s\n" (chor_rpst |> ChoreographyRPST.to_string);
  assert_equal 5 ~printer:Int.to_string (ChoreographyRPST.fragment_count chor_rpst);
  let projection_traveler = Public.PublicNodes.project (BPMNParser.graph parsed) ~role:"Traveler"
  in Public.PublicNodes.start_node projection_traveler |> Public.PublicNode.to_string |> print_endline;
  let projection_travelagency = Public.PublicNodes.project (BPMNParser.graph parsed) ~role:"TravelAgency"
  in Public.PublicNodes.start_node projection_travelagency |> Public.PublicNode.to_string |> print_endline;
  let ta_rpst = Public.PublicRPST.calculate (Public.PublicNodes.start_node projection_travelagency) in
  assert_equal 5 ~printer:Int.to_string (Public.PublicRPST.fragment_count ta_rpst);
  printf "RPST (public: TravelAgency): %s\n" (ta_rpst |> Public.PublicRPST.to_string);
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

let suite =
  "suite" >:::
  ["basic rpst test" >:: test
  ]

let () =
  run_test_tt_main suite

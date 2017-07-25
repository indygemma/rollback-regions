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
  let projection_traveler = Public.graph_choreography_to_public_transform (BPMNParser.graph parsed) ~role:"Traveler" in
  Public.ChorPublicTransform.start_node projection_traveler |> Public.PublicNode.to_string |> print_endline;

  printf "Projection (TravelAgency): \n";
  let projection_travelagency = Public.graph_choreography_to_public_transform (BPMNParser.graph parsed) ~role:"TravelAgency" in
  Public.ChorPublicTransform.start_node projection_travelagency |> Public.PublicNode.to_string |> print_endline;
  let ta_rpst = Public.PublicRPST.calculate (Public.ChorPublicTransform.start_node projection_travelagency) in
  assert_equal 5 ~printer:Int.to_string (Public.PublicRPST.fragment_count ta_rpst);
  printf "RPST (public: TravelAgency): %s\n" (ta_rpst |> Public.PublicRPST.to_string);

  let travelagency_private = Private.graph_public_to_private_transform (Public.ChorPublicTransform.start_node projection_travelagency) in
  let ta_rpst_private = Private.PrivateRPST.calculate (Private.PublicPrivateTransform.start_node travelagency_private) in
  assert_equal 5 ~printer:Int.to_string (Private.PrivateRPST.fragment_count ta_rpst_private);
  printf "RPST (private: TravelAgency): %s\n" (ta_rpst_private |> Private.PrivateRPST.to_string);

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

(*
 * Copyright (c) 2017 Conrad Indiono
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
 * PARTICULAR PURPOSE.  See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program (see file COPYING). If not, see <http://www.gnu.org/licenses/>.
 *)

open Core
open Common
open Choreography
open OUnit2
open Utils

let test test_ctxt =
  let parsed = BPMNParser.parse ~filename:"BookTripOperation.xml" in
  (*print_endline "======";*)
  (*print_endline @@ BPMNParser.to_string parsed;*)
  (*let chor_rpst = ChoreographyRPST.calculate (BPMNParser.graph parsed) in*)
  (*[>printf "RPST (new module): %s\n" (chor_rpst |> ChoreographyRPST.to_string);<]*)
  (*assert_equal 5 ~printer:Int.to_string (ChoreographyRPST.fragment_count chor_rpst);*)
  (*let projection_traveler = Public.graph_choreography_to_public_transform (BPMNParser.graph parsed) ~role:"Traveler" in*)
  (*Public.ChorPublicTransform.start_node projection_traveler |> Public.PublicNode.to_string |> print_endline;*)

  (*[>printf "Projection (TravelAgency): \n";<]*)
  (*let projection_travelagency = Public.graph_choreography_to_public_transform (BPMNParser.graph parsed) ~role:"TravelAgency" in*)
  (*Public.ChorPublicTransform.start_node projection_travelagency |> Public.PublicNode.to_string |> print_endline;*)
  (*let ta_rpst = Public.PublicRPST.calculate (Public.ChorPublicTransform.start_node projection_travelagency) in*)
  (*assert_equal 5 ~printer:Int.to_string (Public.PublicRPST.fragment_count ta_rpst);*)
  (*printf "RPST (public: TravelAgency): %s\n" (ta_rpst |> Public.PublicRPST.to_string);*)

  (*let travelagency_private = Private.graph_public_to_private_transform (Public.ChorPublicTransform.start_node projection_travelagency) in*)
  (*let ta_rpst_private = Private.PrivateRPST.calculate (Private.PublicPrivateTransform.start_node travelagency_private) in*)
  (*assert_equal 5 ~printer:Int.to_string (Private.PrivateRPST.fragment_count ta_rpst_private);*)
  (*printf "RPST (private: TravelAgency): %s\n" (ta_rpst_private |> Private.PrivateRPST.to_string);*)

  (* ALL roles: Traveler, TravelAgency, Acquirer, Airline *)
  let all_roles = ["Traveler"; "TravelAgency"; "Acquirer"; "Airline"] in

  (* Step 1: create all partner public/private models with random private activities *)
  let role_models = List.fold ~init:[]
      ~f:(fun acc role ->
        let public_model = Public.graph_choreography_to_public_transform (BPMNParser.graph parsed) ~role:role in
        let private_model = Private.graph_public_to_private_transform (Public.ChorPublicTransform.start_node public_model) in
        let private_model', start_node = private_model |> Private.add_random_private_activities role 1 in
        let rpst = Private.PrivateRPST.calculate start_node in
        printf "(ROLE: %s) %s\n" role (Private.PrivateRPST.to_string rpst);
        (role, public_model, private_model', start_node) :: acc)
        all_roles
  in

  (* Step 2: create X number of choreography instances, group together business processes and set correct activity states accordingly *)
  let x = 10 in
  let choreography_instances = List.map ~f:(fun _ -> ChoreographyInstance.create_random role_models) (1--x) in
  (* Step 3a: call default CRR (LRR) to estimate the dynamic change impact on all choreography instances *)
  let cost_default = RollbackRegion.crr choreography_instances in
  (* Step 3b: call public LRR to estimate the dynamic change impact on all choreography instances without communication overhead *)
  (*let cost_prr = RollbackRegion.prr choreography_instances in*)
  (* Step 3c: call public LRR + public branching probabilities,
      * # of private activities in each fragment,
      * avg. compensation cost per fragment
      * to estimate the dynamic change impact on all choreography instances
      * without communication overhead *)
  let cost_prr_improved = RollbackRegion.prr_improved choreography_instances in

  printf "CRR: %.f\n" cost_default;
  (*printf "PRR: %.f\n" cost_prr;*)
  printf "PRR+: %.f\n" cost_prr_improved;

  ()

let suite =
  "suite" >:::
  ["basic rpst test" >:: test
  ]

let () =
  run_test_tt_main suite

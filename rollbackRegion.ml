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

let lrr change_region private_instance private_start_node =
    let cur = Private.current_activity private_start_node in
    let start_cr = Private.ChangeRegion.start change_region in
    if not (Private.is_past_activated start_cr private_instance private_start_node) then
        0.0
    else
    (* step 1: determine terminal node *)
    let terminal_node = List.fold ~init:None
        ~f:(fun tn f -> if Private.PrivateRPST.fragment_blocking_probability f = 1.0 then Some f else tn)
        (Private.PrivateRPST.top_fragments cur private_instance private_start_node)
    in
    (* step 2: calculate absolute probabilities *)
    let residual = 1.0 in
    let probs = [] in
    let subfrags = [] in
    let (_, probs, _) = List.fold ~init:(residual, probs, subfrags)
        ~f:(fun (residual, probs, subfrags) f ->
            let (probs', subfrags') = List.fold ~init:([], subfrags)
                ~f:(fun (acc, subfrags') node ->
                    if Private.PrivateNode.is_blocking node && not (List.exists ~f:(fun x -> x = node) subfrags') then
                        ((node, (Private.PrivateRPST.reachability f node private_instance) *. residual) :: acc, node::subfrags')
                    else
                        (acc, subfrags'))
                (Private.PrivateRPST.extract_nodes f)
            in
            (residual *. (1.0 -. Private.PrivateRPST.fragment_blocking_probability f), probs', subfrags'))
        (PrivateRPST.top_fragments_max cur private_instance private_start_node terminal_node)
    in
    (* step 3: calculate expected cost *)
    let fixed = Private.PrivateNode.total_cost start_cr cur private_instance in
    let expected_blocking_cost = List.fold ~init:0.0
        ~f:(fun total_cost (bnode, prob) ->
            total_cost +. (Private.PrivateNode.total_cost cur bnode private_instance) *. prob)
        probs
    in
    fixed +. expected_blocking_cost

let crr change_region choreography_instances =
    List.fold ~init:0.0
        ~f:(fun acc (role, public_instance, private_instance, private_start_node) ->
            acc +. (lrr change_region private_instance private_start_node))
        choreography_instances

let prr' change_region public_instance =
    let cur = Public.current_activity public_instance in
    let start_cr = Public.ChangeRegion.start change_region in
    if not (Public.is_past_activated start_cr public_instance) then
        0.0
    else
    (* step 1: determine terminal node *)
    let terminal_node = List.fold ~init:None
        ~f:(fun tn f -> if Public.PublicRPST.fragment_blocking_probability f = 1.0 then Some f else tn)
        (Public.PublicRPST.top_fragments cur public_instance)
    in
    (* step 2: calculate absolute probabilities *)
    let residual = 1.0 in
    let probs = [] in
    let subfrags = [] in
    let (_, probs, _) = List.fold ~init:(residual, probs, subfrags)
        ~f:(fun (residual, probs, subfrags) f ->
            let (probs', subfrags') = List.fold ~init:([], subfrags)
                ~f:(fun (acc, subfrags') node ->
                    if Public.PublicNode.is_blocking node && not (List.exists ~f:(fun x -> x = node) subfrags') then
                        ((f, node, (Public.PublicRPST.reachability f node public_instance) *. residual) :: acc, node::subfrags')
                    else
                        (acc, subfrags'))
                (Public.PublicRPST.extract_nodes f)
            in
            (residual *. (1.0 -. Public.PublicRPST.fragment_blocking_probability f), probs', subfrags'))
        (PublicRPST.top_fragments_max cur public_instance terminal_node)
    in
    (* step 3: calculate expected cost, abstracted via average cost of private activities inside a fragment + number of private activities in fragment *)
    let fixed = Public.PublicNode.total_cost start_cr cur public_instance in
    let expected_blocking_cost = List.fold ~init:0.0
        ~f:(fun total_cost (frag, bnode, prob) ->
            total_cost +. (Public.PublicRPST.estimate_cost frag cur bnode public_instance) *. prob)
        probs
    in
    fixed +. expected_blocking_cost

let prr_improved change_region choreography_instances =
    List.fold ~init:0.0
        ~f:(fun acc (role, public_instance, private_instance, private_start_node) ->
            acc +. (prr' change_region public_instance))
        choreography_instances

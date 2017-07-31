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

let create_random role_models =
    (* role models = [(role, public model, private model, private model start node)] *)
    List.map ~f:(fun (role, public_model, private_model, private_model_sn) ->
        let private_model', private_model_sn' = Private.randomize_state private_model private_model_sn
                          |> Private.assign_random_brancking_probabilities in
        (* private to public transform takes into consideration the activity states + branching probabilities *)
        let public_model' = Private.PrivateToPublicTransform.transform private_model' private_model_sn' in
        (role, public_model', private_model', private_model_sn'))

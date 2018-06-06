(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: agrumbac <agrumbac@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/06/06 16:44:40 by agrumbac          #+#    #+#             *)
(*   Updated: 2018/06/06 18:29:38 by agrumbac         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let () =



	let doc_army = new Army.army ([new Doctor.doctor ("doc") (42);new Doctor.doctor ("dac") (42);new Doctor.doctor ("dic") (42)]) in
	let new_doc_army = doc_army#add (new Doctor.doctor ("duc") (42)) in
	let _ = new_doc_army#delete in


	let dal_army = new Army.army ([new Dalek.dalek ;new Dalek.dalek ;new Dalek.dalek ]) in
	let new_dal_army = dal_army#add (new Dalek.dalek ) in
	let _ = new_dal_army#delete in


	let ppl_army = new Army.army ([new People.people ("guy");new People.people ("goy");new People.people ("giy")]) in
	let new_ppl_army = ppl_army#add (new People.people ("gay")) in
	let _ = new_ppl_army#delete in
	print_endline ""

(*• You have to simulate a construction and a destruction of an army of each type in the main to provide sufficient testing for the defence. *)

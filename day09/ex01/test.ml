(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   test.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: agrumbac <agrumbac@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/06/08 14:20:39 by agrumbac          #+#    #+#             *)
(*   Updated: 2018/06/08 14:40:26 by agrumbac         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let () =
	let print_proj (a:App.App.project) :unit =
		let (product_a, status_a, grade_a) = a
		in print_endline (product_a ^ ": " ^ status_a ^ " [" ^ (string_of_int grade_a) ^ "]")
	in
	let z:App.App.project = App.App.zero in
	let p:App.App.project = ("Piscine_OCaml", "succeed", 100) in
	let f:App.App.project = App.App.fail p in
	let s:App.App.project = App.App.success p in
	let c:App.App.project = App.App.combine s f in

	print_proj z;
	print_proj p;
	print_proj f;
	print_proj s;
	print_proj c

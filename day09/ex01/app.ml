(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   app.ml                                             :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: agrumbac <agrumbac@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/06/08 14:01:51 by agrumbac          #+#    #+#             *)
(*   Updated: 2018/06/08 14:20:30 by agrumbac         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

module App =
struct
	type project = string * string * int

	let zero:project = ("", "", 0)

	let combine (a:project) (b:project) :project =
		let (product_a, status_a, grade_a) = a
		and (product_b, status_b, grade_b) = b in
		let product_c = product_a ^ product_b
		and grade_c = (grade_a + grade_b) / 2 in
		let status_c = if grade_c > 80 then "succeed" else "failed"
		in (product_c, status_c, grade_c)

	let fail (a:project) :project =
		let (product_a, status_a, grade_a) = a
		in (product_a, "failed", 0)

	let success (a:project) :project =
		let (product_a, status_a, grade_a) = a
		in (product_a, "succeed", 80)

end

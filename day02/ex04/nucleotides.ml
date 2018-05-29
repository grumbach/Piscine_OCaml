(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   nucleotides.ml                                     :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: agrumbac <agrumbac@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/05/30 17:34:02 by agrumbac          #+#    #+#             *)
(*   Updated: 2018/05/30 20:13:29 by agrumbac         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type phosphate = string
type deoxyribose = string
type nucleobase = A | T | C | G | None

type nucleotide =
{
	phosphate: phosphate;
	deoxyribose: deoxyribose;
	nucleobase: nucleobase;
}

let generate_nucleotide type_char =
	let base =
		match type_char with
		| 'A' -> A
		| 'T' -> T
		| 'C' -> C
		| 'G' -> G
		|  _  -> None
	in let new_nucleotide =
	{
		phosphate   = "phosphate";
		deoxyribose = "deoxyribose";
		nucleobase  = base
	}
	in new_nucleotide

(* ********************************** test ********************************** *)

let () =
	let a = generate_nucleotide 'A'
	in
		print_endline (a.phosphate);
		print_endline (a.deoxyribose)

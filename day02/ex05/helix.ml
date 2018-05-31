(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   helix.ml                                           :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: agrumbac <agrumbac@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/05/30 17:34:02 by agrumbac          #+#    #+#             *)
(*   Updated: 2018/05/31 18:00:19 by agrumbac         ###   ########.fr       *)
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
		| 'A'       -> A
		| 'T' | 'B' -> T
		| 'C'       -> C
		| 'G' | 'D' -> G
		|  _  -> None
	in let new_nucleotide =
	{
		phosphate   = "phosphate";
		deoxyribose = "deoxyribose";
		nucleobase  = base
	}
	in new_nucleotide

(* ********************************** nucleotides *************************** *)


type helix = nucleotide list

let rec generate_helix n :helix =
	if n <= 0 then
		[]
	else
		let _ = Random.self_init ()
		and rand_char = char_of_int (65 + (Random.int 4))
		in generate_nucleotide rand_char :: generate_helix (n - 1)

let rec helix_to_string (h:helix) =
	match h with
	| [] -> ""
	| hd :: tl ->
	let base =
		match hd.nucleobase with
		| A -> "A"
		| T -> "T"
		| C -> "C"
		| G -> "G"
		| None -> "_"
	in base ^ (helix_to_string tl)

let rec complementary_helix (h:helix) :helix =
	match h with
	| [] -> []
	| hd :: tl ->
	let complementary_base =
		match hd.nucleobase with
		| A -> generate_nucleotide 'T'
		| T -> generate_nucleotide 'A'
		| C -> generate_nucleotide 'G'
		| G -> generate_nucleotide 'C'
		| None -> generate_nucleotide '_'
	in complementary_base :: complementary_helix tl

(* ********************************** test ********************************** *)

let () =
	let hel = generate_helix 8
	and hel2 = generate_helix 42
	in
	print_endline "__helix:";
	print_endline (helix_to_string hel);
	print_endline "__complementary:";
	print_endline (helix_to_string (complementary_helix hel));
	print_endline "__helix:";
	print_endline (helix_to_string hel2);
	print_endline "__complementary:";
	print_endline (helix_to_string (complementary_helix hel2));
	print_endline "__errors:";
	print_endline (helix_to_string (generate_helix (-1)));
	print_endline (helix_to_string (generate_helix 0));
	print_endline (helix_to_string []);
	print_endline (helix_to_string (complementary_helix []))

(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   Card.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: agrumbac <agrumbac@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/06/01 18:04:25 by agrumbac          #+#    #+#             *)
(*   Updated: 2018/06/01 21:14:31 by agrumbac         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

(************************************ Color ***********************************)

module Color =
struct
	type t = Spade | Heart | Diamond | Club

	let all = [Spade; Heart; Diamond; Club]

	let toString col =
		match col with
		| Spade -> "S"
		| Heart -> "H"
		| Diamond -> "D"
		| Club -> "C"

	let toStringVerbose col =
		match col with
		| Spade -> "Spade"
		| Heart -> "Heart"
		| Diamond -> "Diamond"
		| Club -> "Club"
end

(************************************ Value ***********************************)

module Value =
struct
	type t = T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | Jack | Queen | King | As

	let all = [T2; T3; T4; T5; T6; T7; T8; T9; T10; Jack; Queen; King; As]

	let toInt value =
		match value with
		| T2    -> 1
		| T3    -> 2
		| T4    -> 3
		| T5    -> 4
		| T6    -> 5
		| T7    -> 6
		| T8    -> 7
		| T9    -> 8
		| T10   -> 9
		| Jack  -> 10
		| Queen -> 11
		| King  -> 12
		| As    -> 13

	let toString value =
		match value with
		| T2    -> "2"
		| T3    -> "3"
		| T4    -> "4"
		| T5    -> "5"
		| T6    -> "6"
		| T7    -> "7"
		| T8    -> "8"
		| T9    -> "9"
		| T10   -> "10"
		| Jack  -> "J"
		| Queen -> "Q"
		| King  -> "K"
		| As    -> "A"

	let toStringVerbose value =
		match value with
		| T2    -> "2"
		| T3    -> "3"
		| T4    -> "4"
		| T5    -> "5"
		| T6    -> "6"
		| T7    -> "7"
		| T8    -> "8"
		| T9    -> "9"
		| T10   -> "10"
		| Jack  -> "Jack"
		| Queen -> "Queen"
		| King  -> "King"
		| As    -> "As"

	let next value =
		match value with
		| T2    -> T3
		| T3    -> T4
		| T4    -> T5
		| T5    -> T6
		| T6    -> T7
		| T7    -> T8
		| T8    -> T9
		| T9    -> T10
		| T10   -> Jack
		| Jack  -> Queen
		| Queen -> King
		| King  -> As
		| As    -> invalid_arg "Nothing is after As"

	let prev value =
		match value with
		| T2    -> invalid_arg "Nothing is before 2"
		| T3    -> T2
		| T4    -> T3
		| T5    -> T4
		| T6    -> T5
		| T7    -> T6
		| T8    -> T7
		| T9    -> T8
		| T10   -> T9
		| Jack  -> T10
		| Queen -> Jack
		| King  -> Queen
		| As    -> King
end

(************************************ Card ************************************)

type t =
{
	color:Color.t;
	value:Value.t
}

let newCard v c =
	{color = c; value = v}

let allSpades =
	let v = Value.all in
		let rec get_next_card va = match va with
		| [] -> []
		| (head:Value.t)::tail -> ({ color = Color.Spade; value = head}) :: (get_next_card tail)
	in get_next_card v

let allHearts =
	let v = Value.all in
		let rec get_next_card va = match va with
		| [] -> []
		| (head:Value.t)::tail -> ({ color = Color.Heart; value = head}) :: (get_next_card tail)
	in get_next_card v

let allDiamonds =
	let v = Value.all in
		let rec get_next_card va = match va with
		| [] -> []
		| (head:Value.t)::tail -> ({ color = Color.Diamond; value = head}) :: (get_next_card tail)
	in get_next_card v

let allClubs =
	let v = Value.all in
		let rec get_next_card va = match va with
		| [] -> []
		| (head:Value.t)::tail -> ({ color = Color.Club; value = head}) :: (get_next_card tail)
	in get_next_card v

let all = allSpades@allClubs@allDiamonds@allHearts

(* get *)

let getValue card = card.value

let getColor card = card.color

let toString card =
	((Value.toString card.value) ^ (Color.toString card.color))

let toStringVerbose card =
	("Card(" ^ (Value.toStringVerbose card.value) ^ ", " ^ (Color.toStringVerbose card.color) ^ ")")

(* compare *)

let compare card1 card2 =
		(Value.toInt card1.value) - (Value.toInt card2.value)

let max c1 c2 =
	let cmp = compare c1 c2 in
	if cmp < 0 then
		c2
	else
		c1

let min c1 c2 =
	let cmp = compare c1 c2 in
	if cmp > 0 then
		c2
	else
		c1

let best lst =
	match lst with
	| hd :: [] -> hd
	| hd :: tl -> List.fold_left max hd tl
	| [] -> invalid_arg "Must at least provide 1 card"

(* type identifier *)

let isOf (card:t) (color:Color.t) = (card.color = color)

let isSpades   card =
	isOf card Color.Spade

let isHearts   card =
	isOf card Color.Heart

let isDiamonds card =
	isOf card Color.Diamond

let isClubs    card =
	isOf card Color.Club

(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: agrumbac <agrumbac@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/06/01 16:40:03 by agrumbac          #+#    #+#             *)
(*   Updated: 2018/06/01 21:43:56 by agrumbac         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let () =
	let card1:Card.t = (Card.newCard Card.Value.T9 Card.Color.Heart)
	and card2:Card.t = (Card.newCard Card.Value.King Card.Color.Spade)
	and card3:Card.t = (Card.newCard Card.Value.Queen Card.Color.Diamond)
	and card4:Card.t = (Card.newCard Card.Value.T2 Card.Color.Club)
	in let best_card:Card.t = Card.best [card1; card2; card3; card4]
	in
	print_endline ("card  :" ^ (Card.toStringVerbose card1));
	print_endline ("card  :" ^ (Card.toStringVerbose card2));
	print_endline ("card  :" ^ (Card.toStringVerbose card3));
	print_endline ("card  :" ^ (Card.toStringVerbose card4));
	print_endline ("the best :" ^ (Card.toString best_card));
	print_endline "--------------";
	let () =
	if Card.isHearts card1 then
		print_endline ("this card is Hearts  :" ^ (Card.toString card1))
	else
		print_endline ("this card is not Hearts  :" ^ (Card.toString card1))
	and () =
	if Card.isSpades card1 then
		print_endline ("this card is Spades  :" ^ (Card.toString card1))
	else
		print_endline ("this card is not Spades  :" ^ (Card.toString card1))
	and () =
	if Card.isDiamonds card1 then
		print_endline ("this card is Diamonds  :" ^ (Card.toString card1))
	else
		print_endline ("this card is not Diamonds  :" ^ (Card.toString card1))
	and () =
	if Card.isClubs card1 then
		print_endline ("this card is Clubs  :" ^ (Card.toString card1))
	else
		print_endline ("this card is not Clubs  :" ^ (Card.toString card1))
	in
	print_endline "--------------";
	print_endline ("card value: " ^ (Card.Value.toString (Card.getValue card1)));
	print_endline ("card color: " ^ (Card.Color.toString (Card.getColor card1)));
	print_endline "--------------";
	print_endline ("Best of all: " ^ (Card.toString (Card.best Card.all)))

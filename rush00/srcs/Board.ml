(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   Board.ml                                           :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: agrumbac <agrumbac@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/06/02 17:25:37 by agrumbac          #+#    #+#             *)
(*   Updated: 2018/06/02 19:00:43 by agrumbac         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type coordinates = int * int

type symbol = O | X | None

(* board of 9 cell containing 9 cell *)

type cell = Symbol of symbol | Board of board

and board = cell * cell * cell
		  * cell * cell * cell
		  * cell * cell * cell

(* let add_player_move (board:board) (move:coordinates)	:board = *)

(* let symbol_board_of_board (board:board)						:board = *)

(* let resolve (symbol_board:board)						:symbol = *)

let big_X = ("\\   /",
			 "  X  ",
			 "/   \\")

let big_O = ("/ - \\",
			 "|   |",
			 "\\ - /")

let big_None = ("- - -",
				"- - -",
				"- - -")

let char_of_symbol symbol =
	match symbol with
	| O    -> 'O'
	| X    -> 'X'
	| None -> '-'

let string_of_board_rows left center right =
	let cell_to_string c =
		match c with
		| Symbol (O) -> big_O
		| Symbol (X) -> big_X
		| Symbol (None) -> big_None
		| Board  (b) -> ("1 1 1", "2 2 2", "3 3 3")
	in
	let (left_top, left_mid, left_bot) = cell_to_string left
	and (center_top, center_mid, center_bot) = cell_to_string center
	and (right_top, right_mid, right_bot) = cell_to_string right
	in
		left_top ^ " | " ^ center_top ^ " | " ^ right_top ^ "\n" ^
		left_mid ^ " | " ^ center_mid ^ " | " ^ right_mid ^ "\n" ^
		left_bot ^ " | " ^ center_bot ^ " | " ^ right_bot ^ "\n"

let display_board (board:board) 					:unit =
	let (left_top, left_mid, left_bot, center_top, center_mid, center_bot, right_top, right_mid, right_bot) = board
	in
	begin
		print_string (string_of_board_rows left_top left_mid left_bot);
		print_string "---------------------\n";
		print_string (string_of_board_rows center_top center_mid center_bot);
		print_string "---------------------\n";
		print_string (string_of_board_rows right_top right_mid right_bot);
	end


(* kek TODO *)

let () =
	display_board (Symbol(O) , Symbol(X) , Symbol(None),
						 Symbol(O) , Symbol(X) , Symbol(None),
						 Symbol(O) , Symbol(X) , Symbol(None));

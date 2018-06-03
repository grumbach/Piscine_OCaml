(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   Board.ml                                           :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: agrumbac <agrumbac@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/06/02 17:25:37 by agrumbac          #+#    #+#             *)
(*   Updated: 2018/06/03 01:03:21 by agrumbac         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

(* ---------------------------- types --------------------------------------- *)

type coordinate = int | Incorrect

type coordinates = Valid of coordinate * coordinate | Invalid of string

type symbol = O | X | None

(* board of 9 cell containing 9 symbol *)

type cell = symbol * symbol * symbol
		  * symbol * symbol * symbol
		  * symbol * symbol * symbol

type board = cell * cell * cell
		   * cell * cell * cell
		   * cell * cell * cell

(* ---------------------------- add_player_move ----------------------------- *)

(* TODO add_player_move *)
(* TODO add_player_move makes board full of X if X puts last symbol into
the grid (no other way to know who played last....) *)

(* let add_player_move (board:board) (move:coordinates)	:board = *)

(* ---------------------------- resolve ------------------------------------- *)

let resolve (cell:cell)					:symbol =
	let check_if_symbol_X_won =
		match cell with
		| (X, X, X, _, _, _, _, _, _) -> true
		| (_, _, _, X, X, X, _, _, _) -> true
		| (_, _, _, _, _, _, X, X, X) -> true
		| (_, _, X, _, _, X, _, _, X) -> true
		| (_, X, _, _, X, _, _, X, _) -> true
		| (X, _, _, X, _, _, X, _, _) -> true
		| (X, _, _, _, X, _, _, _, X) -> true
		| (_, _, X, _, X, _, X, _, _) -> true
		| _ -> false
	and check_if_symbol_O_won =
		match cell with
		| (O, O, O, _, _, _, _, _, _) -> true
		| (_, _, _, O, O, O, _, _, _) -> true
		| (_, _, _, _, _, _, O, O, O) -> true
		| (_, _, O, _, _, O, _, _, O) -> true
		| (_, O, _, _, O, _, _, O, _) -> true
		| (O, _, _, O, _, _, O, _, _) -> true
		| (O, _, _, _, O, _, _, _, O) -> true
		| (_, _, O, _, O, _, O, _, _) -> true
		| _ -> false
	in
	if check_if_symbol_X_won then
		X
	else if check_if_symbol_O_won then
		O
	else
		None


(* ---------------------------- display_board ------------------------------- *)

let big_X = ("\\   /",
			 "  X  ",
			 "/   \\")

let big_O = ("/ - \\",
			 "|   |",
			 "\\ - /")

let str_of_sym symbol =
	match symbol with
	| O    -> "O"
	| X    -> "X"
	| None -> "-"

let string_of_cell cell =
	let (lt, lm, lb, ct, cm, cb, rt, rm, rb) = cell
	in
		((str_of_sym lt) ^ " " ^ (str_of_sym lm) ^ " " ^ (str_of_sym lb),
		(str_of_sym ct) ^ " " ^ (str_of_sym cm) ^ " " ^ (str_of_sym cb),
		(str_of_sym rt) ^ " " ^ (str_of_sym rm) ^ " " ^ (str_of_sym rb))

let string_of_cells left center right =
	let get_cell_string c =
		let r = (resolve c) in
		match r with
		| O -> big_O
		| X -> big_X
		| None -> string_of_cell c
	in
	let (left_top, left_mid, left_bot) = get_cell_string left
	and (center_top, center_mid, center_bot) = get_cell_string center
	and (right_top, right_mid, right_bot) = get_cell_string right
	in
		left_top ^ " | " ^ center_top ^ " | " ^ right_top ^ "\n" ^
		left_mid ^ " | " ^ center_mid ^ " | " ^ right_mid ^ "\n" ^
		left_bot ^ " | " ^ center_bot ^ " | " ^ right_bot ^ "\n"

let display_board (board:board) :unit =
	let (left_top, left_mid, left_bot, center_top, center_mid, center_bot, right_top, right_mid, right_bot) = board
	in
	begin
		print_string (string_of_cells left_top left_mid left_bot);
		print_string "---------------------\n";
		print_string (string_of_cells center_top center_mid center_bot);
		print_string "---------------------\n";
		print_string (string_of_cells right_top right_mid right_bot);
	end

(* ---------------------------- end ----------------------------------------- *)

(* TODO put this in main!! *)

let () =
	let bobo = ((O) , (O) , (None),
				(O) , (X) , (None),
				(O) , (X) , (None))
	and boba = ((O) , (None) , (None),
				(X) , (X) , (X),
				(O) , (X) , (None))
	and bobi = ((O) , (None) , (None),
				(X) , (None) , (X),
				(O) , (X) , (None))
	in
		display_board (bobi, boba, bobo,
						bobo, bobo, bobo,
						bobo, bobo, bobi)

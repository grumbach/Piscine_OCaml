(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   Board.ml                                           :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: agrumbac <agrumbac@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/06/02 17:25:37 by agrumbac          #+#    #+#             *)
(*   Updated: 2018/06/03 17:55:01 by agrumbac         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

(* ---------------------------- types --------------------------------------- *)

type coordinate = Correct of int | Incorrect

type coordinates = Pair of coordinate * coordinate | Invalid of string

type symbol = O | X | None

(* board of 9 cell containing 9 symbol *)

type cell = symbol * symbol * symbol
		  * symbol * symbol * symbol
		  * symbol * symbol * symbol

type board = cell * cell * cell
		   * cell * cell * cell
		   * cell * cell * cell


let coordinate_proj c = match c with
  | Correct n -> n
  | Incorrect -> -1

(* ---------------------------- resolve ------------------------------------- *)

let resolve (cell:cell)									:symbol =
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

(* ---------------------------- is_there_a_winner --------------------------- *)

let is_there_a_winner (board:board) :symbol =
	None

(* ---------------------------- newBoard ------------------------------------ *)

let new_board () =
  let empty:cell = ((None), (None), (None),
					(None), (None), (None),
					(None), (None), (None))
	in
	(empty, empty, empty,
	 empty, empty, empty,
	 empty, empty, empty)

(* ---------------------------- is_move_available --------------------------- *)

let get_123_range n =
	let range = n mod 3 in
	match range with
	| 0 -> 3
	| _ -> range

let get_cell_number_with x y =
	let match_position_cell n =
		match n with
		| n when n <= 3 -> 1
		| n when n <= 6 -> 2
		| _ -> 3
	in (3 * ((match_position_cell y) - 1) + (match_position_cell x))

let is_move_available (board:board) (x:int) (y:int)				:bool =
	let is_cell_symbol_none (cell:cell) (cell_y:int) (cell_x:int) =
		let index = 3 * (cell_y - 1) + cell_x in
		match cell with
		| (None, _, _, _, _, _, _, _, _) when index = 1 -> true
		| (_, None, _, _, _, _, _, _, _) when index = 2 -> true
		| (_, _, None, _, _, _, _, _, _) when index = 3 -> true
		| (_, _, _, None, _, _, _, _, _) when index = 4 -> true
		| (_, _, _, _, None, _, _, _, _) when index = 5 -> true
		| (_, _, _, _, _, None, _, _, _) when index = 6 -> true
		| (_, _, _, _, _, _, None, _, _) when index = 7 -> true
		| (_, _, _, _, _, _, _, None, _) when index = 8 -> true
		| (_, _, _, _, _, _, _, _, None) when index = 9 -> true
		| _ -> false
	in
	let (c1, c2, c3, c4, c5, c6, c7, c8, c9) = board
	and cell_number = get_cell_number_with x y
	and cell_x_pos = get_123_range x
	and cell_y_pos = get_123_range y
	in
	match cell_number with
	| 1 when resolve c1 = None -> is_cell_symbol_none c1 cell_y_pos cell_x_pos
	| 2 when resolve c2 = None -> is_cell_symbol_none c2 cell_y_pos cell_x_pos
	| 3 when resolve c3 = None -> is_cell_symbol_none c3 cell_y_pos cell_x_pos
	| 4 when resolve c4 = None -> is_cell_symbol_none c4 cell_y_pos cell_x_pos
	| 5 when resolve c5 = None -> is_cell_symbol_none c5 cell_y_pos cell_x_pos
	| 6 when resolve c6 = None -> is_cell_symbol_none c6 cell_y_pos cell_x_pos
	| 7 when resolve c7 = None -> is_cell_symbol_none c7 cell_y_pos cell_x_pos
	| 8 when resolve c8 = None -> is_cell_symbol_none c8 cell_y_pos cell_x_pos
	| 9 when resolve c9 = None -> is_cell_symbol_none c9 cell_y_pos cell_x_pos
	| _ -> false

(* ---------------------------- get_cell_from_coordinates ------------------- *)

let get_cell_from_coordinates board x y :cell =
	let (c1, c2, c3, c4, c5, c6, c7, c8, c9) = board
	and cn = get_cell_number_with x y in
	match cn with
	| 1 -> c1
	| 2 -> c2
	| 3 -> c3
	| 4 -> c4
	| 5 -> c5
	| 6 -> c6
	| 7 -> c7
	| 8 -> c8
	| _ -> c9

(* ---------------------------- add_player_move ----------------------------- *)

let generate_sym (replace:bool) (old_symbol:symbol) (new_symbol:symbol) :symbol =
	if replace then
		new_symbol
	else
		old_symbol

let is_cell_full (cell:cell) :bool =
	match cell with
	| (None, _, _, _, _, _, _, _, _) -> false
	| (_, None, _, _, _, _, _, _, _) -> false
	| (_, _, None, _, _, _, _, _, _) -> false
	| (_, _, _, None, _, _, _, _, _) -> false
	| (_, _, _, _, None, _, _, _, _) -> false
	| (_, _, _, _, _, None, _, _, _) -> false
	| (_, _, _, _, _, _, None, _, _) -> false
	| (_, _, _, _, _, _, _, None, _) -> false
	| (_, _, _, _, _, _, _, _, None) -> false
	| _ -> true

let add_move_to_cell (cell:cell) (x:int) (y:int) (s:symbol) :cell =
	let (s1, s2, s3, s4, s5, s6, s7, s8, s9) = cell
	and pos = 3 * (y - 1) + x
	in
	let new_cell = ((generate_sym (1 = pos) s1 s), (generate_sym (2 = pos) s2 s),
					(generate_sym (3 = pos) s3 s),
					(generate_sym (4 = pos) s4 s), (generate_sym (5 = pos) s5 s),
					(generate_sym (6 = pos) s6 s),
					(generate_sym (7 = pos) s7 s), (generate_sym (8 = pos) s8 s),
					(generate_sym (9 = pos) s9 s))
	in
	if is_cell_full new_cell then (* s won cell x y ! *)
		(s, s, s, s, s, s, s, s, s)
	else
		new_cell

let generate_cell (cell:cell) (keep:bool) (x:int) (y:int) (symbol:symbol) :cell=
	if keep then
		cell
	else
		add_move_to_cell cell x y symbol

let add_player_move (board:board) (x:int) (y:int) (symbol:symbol)		:board =
	let (c1, c2, c3, c4, c5, c6, c7, c8, c9) = board
	and cn = get_cell_number_with x y
	and px = get_123_range x
	and py = get_123_range y
	in
	((generate_cell c1 (cn <> 1) px py symbol),
	 (generate_cell c2 (cn <> 2) px py symbol),
	 (generate_cell c3 (cn <> 3) px py symbol),
	 (generate_cell c4 (cn <> 4) px py symbol),
	 (generate_cell c5 (cn <> 5) px py symbol),
	 (generate_cell c6 (cn <> 6) px py symbol),
	 (generate_cell c7 (cn <> 7) px py symbol),
	 (generate_cell c8 (cn <> 8) px py symbol),
	 (generate_cell c9 (cn <> 9) px py symbol))

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

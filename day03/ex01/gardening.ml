(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   gardening.ml                                       :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: agrumbac <agrumbac@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/05/31 16:42:23 by agrumbac          #+#    #+#             *)
(*   Updated: 2018/05/31 21:23:01 by agrumbac         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type 'a tree = Nil | Node of 'a * 'a tree * 'a tree

let draw_square xo yo size =
	let x = (xo - (size / 2))
	and y = (yo - (size / 2))
	in
	Graphics.moveto x y;
	Graphics.lineto x (y + size);
	Graphics.lineto (x + size) (y + size);
	Graphics.lineto (x + size) y;
	Graphics.lineto x y

let draw_tree_node node =
	let square_size = 42 in
	let square_half = square_size / 2 in
	match node with
	| Nil -> ()
	| Node (value, left, right) ->
	let rec draw_next_nodes n x y z =
		match n with
		| Nil ->
			draw_square x y square_size;
			Graphics.draw_string "Nil"
		| Node (v, l, r) ->
			draw_square x y square_size;
			Graphics.draw_string value;
			Graphics.moveto   (x + square_half)       y;
			Graphics.lineto   (x + 120 - square_half) (y + z);
			Graphics.moveto   (x + square_half)       y;
			Graphics.lineto   (x + 120 - square_half) (y - z);
			draw_next_nodes l (x + 120)               (y + z)  ((z * 3) / 4);
			draw_next_nodes r (x + 120)               (y - z)  ((z * 3) / 4)
	in draw_next_nodes node 100 450 100

(* ********************************** gardening ***************************** *)

let rec size t =
	match t with
	| Nil -> 0
	| Node (value, left, right) -> 1 + (size left) + (size right)

let rec height t =
	match t with
	| Nil -> 0
	| Node (value, left, right) ->
		let left_size = size left and right_size = size right in
			if left_size > right_size then
				left_size + 1
			else
				right_size + 1

let draw_tree t =
	draw_tree_node t

(* ********************************** test ********************************** *)


let main () =
	Graphics.open_graph " 1400x900";
	let t = (Node ("42", (Node ("lol", (Node ("tu", Nil, Nil)), Nil)), (Node ("ti", Nil, (Node ("to", (Node ("ta", (Node ("3", Nil, (Node ("0", Nil, Nil)))), Nil)), Nil))))))
	in
		draw_tree t;
		Graphics.moveto 10 800;
		Graphics.draw_string ("Size : " ^ (string_of_int (size t)));
		Graphics.moveto 10 700;
		Graphics.draw_string ("Height : " ^ (string_of_int (height t)));
		Graphics.read_key ()

let _ = main ()

(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_graphics.ml                                     :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: agrumbac <agrumbac@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/05/31 16:42:23 by agrumbac          #+#    #+#             *)
(*   Updated: 2018/05/31 21:03:40 by agrumbac         ###   ########.fr       *)
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
			Graphics.draw_string "Value";
			Graphics.moveto   (x + square_half)       y;
			Graphics.lineto   (x + 120 - square_half) (y + z);
			Graphics.moveto   (x + square_half)       y;
			Graphics.lineto   (x + 120 - square_half) (y - z);
			draw_next_nodes l (x + 120)               (y + z)  ((z * 3) / 4);
			draw_next_nodes r (x + 120)               (y - z)  ((z * 3) / 4)
	in draw_next_nodes node 100 450 100


let main () =
	Graphics.open_graph " 1400x900";
	draw_tree_node (Node (42, (Node (42, (Node (42, Nil, Nil)), Nil)), (Node (42, Nil, (Node (42, (Node (42, (Node (42, Nil, (Node (42, Nil, Nil)))), Nil)), Nil))))));
	Graphics.read_key ()

let _ = main ()


(* ********************************** test ********************************** *)

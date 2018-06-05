(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ft_ref.ml                                          :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: agrumbac <agrumbac@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/06/04 16:34:40 by agrumbac          #+#    #+#             *)
(*   Updated: 2018/06/04 19:58:12 by agrumbac         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type 'a ft_ref = {mutable contents: 'a}

let return a = {contents = a}

let get r = r.contents

let set r a = ignore (r.contents <- a)

let bind r f =
	return (f (get r))

let () =
	let a = return "4" in
	let b = bind a int_of_string in
	print_string "string:       "; print_endline (get a);
	print_string "int:          "; print_int (get b);
	print_endline "";
	set b 42;
	print_string "modified int: ";  print_int (get b);
	print_endline "";

(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: agrumbac <agrumbac@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/06/02 14:13:34 by agrumbac          #+#    #+#             *)
(*   Updated: 2018/06/03 12:58:56 by agrumbac         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let () =
	let bobo = ((Board.O) , (Board.O) , (Board.None),
				(Board.O) , (Board.X) , (Board.None),
				(Board.O) , (Board.X) , (Board.None))
	and boba = ((Board.O) , (Board.None) , (Board.None),
				(Board.X) , (Board.X) , (Board.X),
				(Board.O) , (Board.X) , (Board.None))
	and bobi = ((Board.O) , (Board.None) , (Board.None),
				(Board.X) , (Board.None) , (Board.X),
				(Board.O) , (Board.X) , (Board.None))
	in
		Board.display_board (bobi, boba, bobo,
							bobo, bobo, bobo,
							bobo, bobo, bobi)

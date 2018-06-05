(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   jokes.ml                                           :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: agrumbac <agrumbac@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/06/04 19:48:32 by agrumbac          #+#    #+#             *)
(*   Updated: 2018/06/04 20:25:26 by agrumbac         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let generate_bullshit i =
	match i with
		| 1 -> "What pizza did the twin towers order? -- two large plains"
		| 2 -> "What's the best thing about Switzerland? -- I don't know, but the flag is a big plus"
		| 3 -> "Did you hear about the mathematician who's afraid of negative numbers? -- He'll stop at nothing to avoid them"
		| 4 -> "Here’s some advice: At a job  interview, tell them you’re willing to give 110 percent. Unless the job is a statistician"
		| _ -> "A farmer counted 196 cows in  the field. But when he rounded them up, he had 200"

let () =
	Random.self_init ();
	let jokes = Array.init 5 generate_bullshit in

	print_endline (Array.get jokes (Random.int 5))

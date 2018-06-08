(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   try.ml                                             :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: agrumbac <agrumbac@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/06/08 19:55:58 by agrumbac          #+#    #+#             *)
(*   Updated: 2018/06/08 21:33:02 by agrumbac         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

module Try =
struct
	type 'a t = Success of 'a | Failure of exn

	let return x = Success x

	let bind x (f:'a -> 'b t) =
		try
			match x with
			| Success x -> f x
			| Failure ex -> Failure ex
		with
		| ex -> Failure ex

	let recover x (f:exn -> 'a t) =
		match x with
		| Failure ex -> f ex
		| _ -> x

	let filter x (cmp:'a -> bool) =
		match x with
		| Failure ex -> Failure ex
		| Success x ->
		if cmp x then
			Failure (Failure "sorted out")
		else
			Success x

	let flatten xx =
		match xx with
		| Failure ex -> Failure ex
		| Success x -> x
end

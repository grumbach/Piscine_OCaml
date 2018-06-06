(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ex02.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: agrumbac <agrumbac@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/06/05 22:21:49 by agrumbac          #+#    #+#             *)
(*   Updated: 2018/06/06 13:46:11 by agrumbac         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

module type PAIR = sig val pair : (int * int) end

module type VAL = sig val x : int end

(*  functors MakeFst and MakeSnd and their signature MAKEPROJECTION *)

module type MAKEPROJECTION =
	functor (Pair : PAIR) -> VAL

module MakeFst : MAKEPROJECTION =
	functor ( Pair : PAIR ) ->
	struct
		let x = fst Pair.pair
	end

module MakeSnd : MAKEPROJECTION =
	functor ( Pair : PAIR ) ->
	struct
		let x = snd Pair.pair
	end

(*  *)

module Pair : PAIR =
	struct
		let pair = ( 21, 42 )
	end

module Fst : VAL = MakeFst (Pair)
module Snd : VAL = MakeSnd (Pair)



let () = Printf.printf "Fst.x = %d, Snd.x = %d\n" Fst.x Snd.x

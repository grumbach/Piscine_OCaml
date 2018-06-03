(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   UserInterface.ml                                   :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: agrumbac <agrumbac@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/06/02 17:25:33 by agrumbac          #+#    #+#             *)
(*   Updated: 2018/06/03 12:58:51 by agrumbac         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

(* let get_user_name (prompt:string)					:string = *)

(* let get_play_again (prompt:string)					:unit = *)

let is_digit c = c >= '0' && c <= '9'

let is_move_valid (move:string) =
  if (String.length move) <> 1
  then false
  else is_digit (String.get move 1)

let digit_of_move (m:string) =
  (int_of_char (String.get m 1)) - (int_of_char '0')

let coordinate_of_move (m:string) =
  if is_move_valid m
  then Board.Correct (digit_of_move m)
  else Board.Incorrect

let coordinates_of_move (move_x:string) (move_y:string) =
  let coordinates = Board.Pair (coordinate_of_move move_x, coordinate_of_move move_y)
  in
  match coordinates with
  | Board.Pair (_, Board.Incorrect)
  | Board.Pair (Board.Incorrect, _)  -> Board.Invalid ("Incorrect format.")
  | _                                -> coordinates

let parse_coordinates (user_move:string) =
  let moves = String.split_on_char ' ' user_move in
  match moves with
  | x::y::[] -> coordinates_of_move x y
  | _        -> Board.Invalid ("Incorrect format")

let get_user_move () =
  let user_move = read_line () in
  parse_coordinates user_move

(* let display_status (message:string)                 :unit = *)

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

(* TODO Change function name *)
let rec handle_player_turn board player =
  let player_move = UserInterface.get_user_move () in
  match player_move with
  | Board.Invalid (msg)
    ->
     begin
       print_endline msg;
       handle_player_turn board player
     end
  | Board.Pair (Board.Incorrect, _) | Board.Pair (_, Board.Incorrect)
    ->
     begin
       print_endline "Incorrect format.";
       handle_player_turn board player
     end
  | Board.Pair (x, y) when Board.is_move_available (x, y)
    ->
     Board.add_player_move board (Board.coordinate_proj x) (Board.coordinate_proj y) player
  | _
    ->
     begin
       print_endline "Illegal move.";
       handle_player_turn board player
     end

(* TODO Change function name *)
let do_player_turn board player =
  begin
    Board.display_board board;
    print_endline ((Board.str_of_sym player) ^ "'s turn to play.");
    handle_player_turn board player
  end

let rec main_loop board player =
  if Board.is_there_a_winner board
  then Board.display_board board
  else
    match player with
    | Board.O -> main_loop (do_player_turn board Board.X) Board.X
    | Board.X -> main_loop (do_player_turn board Board.O) Board.O
    | _       -> main_loop (do_player_turn board player) player

let () = main_loop (Board.new_board ()) Board.X

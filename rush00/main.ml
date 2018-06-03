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
let rec handle_player_turn board =
  let player_move = UserInterface.get_user_move () in
  match player_move with
  | Board.Invalid (msg)
    ->
     begin
       print_endline msg;
       handle_player_turn board
     end
  | Board.Pair (Board.Incorrect, _) | Board.Pair (_, Board.Incorrect)
    ->
     begin
       print_endline "Incorrect format.";
       handle_player_turn board
     end
  | Board.Pair (x, y) when Board.is_move_available (x, y)
    ->
     Board.add_player_move board player_move
  | _
    ->
     begin
       print_endline "Illegal move.";
       handle_player_turn board
     end

(* TODO Change function name *)
let do_player_turn player board =
  begin
    Board.display_board board;
    print_endline (player ^ "'s turn to play.");
    handle_player_turn board
  end

let rec main_loop board =
  if Board.is_there_a_winner board
  then Board.display_board board
  else main_loop (do_player_turn "Joueur1" board)

let () = main_loop (Board.new_board ())

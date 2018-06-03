(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   main.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: agrumbac <agrumbac@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/06/02 14:13:34 by agrumbac          #+#    #+#             *)
(*   Updated: 2018/06/03 19:31:41 by agrumbac         ###   ########.fr       *)
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
  | Board.Pair (x, y) when Board.is_move_available board (Board.coordinate_proj x) (Board.coordinate_proj y)
    ->
     let coor_x = (Board.coordinate_proj x)
     and coor_y = (Board.coordinate_proj y)
     in
     let new_board = Board.add_player_move board coor_x coor_y player
     in
     let cell = Board.get_cell_from_coordinates new_board coor_x coor_y
     and cell_nb = Board.get_cell_number_with coor_x coor_y
     in
     let winning_symbol = Board.resolve cell
     in
     if winning_symbol <> Board.None
     then
       begin
         print_string ((Board.str_of_sym player) ^ " wins grid ");
         print_int cell_nb;
         print_endline "!\n";
         new_board
       end
     else new_board
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
    print_endline ("\n" ^ (Board.str_of_sym player) ^ "'s turn to play.");
    handle_player_turn board player
  end

let rec main_loop board player =
  let winner = Board.is_there_a_winner board
  in
  if winner = Board.Draw
  then
    begin
      print_endline "Draw! No winner";
      Board.display_board board
    end
  else if winner <> Board.Winner (None)
  then
    begin
      print_endline ((Board.str_of_sym player) ^ " wins the game!\n");
      Board.display_board board
    end
  else
    match player with
	| Board.X | Board.None -> main_loop (do_player_turn board Board.O) Board.O
    | Board.O -> main_loop (do_player_turn board Board.X) Board.X

let () = main_loop (Board.new_board ()) Board.None

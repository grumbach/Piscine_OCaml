(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   view.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: agrumbac <agrumbac@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/06/08 23:07:10 by agrumbac          #+#    #+#             *)
(*   Updated: 2018/06/10 13:14:43 by agrumbac         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

(* this manages the view *)

type action = Eat | Thunder | Bath | Kill | Waiting | Retry | Save | Load | Exit


module type GRAPHIC_INTERFACE =
	sig
		val init : unit -> unit
		val draw : action -> Tama.pet -> Tama.TamaMonad.t
		val get_action : unit -> action
		val draw_exit : unit -> unit
		val getTime : unit -> float
	end


module type USER_INTERFACE =
	sig
		val main : unit -> unit
		val apply_action : action -> Tama.TamaMonad.t -> float -> Tama.TamaMonad.t
	end


module type MAKE_USER_INTERFACE =
	functor (Graphic_Interface:GRAPHIC_INTERFACE) -> (USER_INTERFACE)

module Make_User_Interface : MAKE_USER_INTERFACE =
	functor (Graphic_Interface:GRAPHIC_INTERFACE) ->
	struct
		let apply_action act tama_t delta_seconds =
			let bind_tama_t fct =
				Tama.TamaMonad.apply
				(
					Tama.TamaMonad.apply
					(
						Tama.TamaMonad.bind
						tama_t
						(
							Tama.TamaMonad.decrease_health_by_n delta_seconds
						)
					)
					fct
				)
				(Tama.TamaMonad.backup_to "./save.itama")
			in
			match act with
			| Eat 		-> bind_tama_t Tama.TamaMonad.eat
			| Thunder	-> bind_tama_t Tama.TamaMonad.thunder
			| Bath		-> bind_tama_t Tama.TamaMonad.bath
			| Kill		-> bind_tama_t Tama.TamaMonad.kill
			| Save		-> bind_tama_t (Tama.TamaMonad.backup_to "./save.itama_1")
			| Load		-> bind_tama_t (Tama.TamaMonad.recover_from "./save.itama_1")
			| Retry 	-> bind_tama_t (fun x -> Tama.TamaMonad.return (new Tama.pet 100 100 100 100))
			| _			-> bind_tama_t Tama.TamaMonad.return

		let main () =
			let rec main_loop tama_t prev_sec display_action =
				let tama_t =
					Tama.TamaMonad.apply tama_t (fun x -> Graphic_Interface.draw display_action x)
				in let sec 	= Graphic_Interface.getTime () in
				let action 	= Graphic_Interface.get_action () 	in
				match action with
				| Exit -> Graphic_Interface.draw_exit ()
				| _ ->
					main_loop
					(
						apply_action
						(
							action
						)
						tama_t
						(sec -. prev_sec)
					)
					(
						if (sec -. prev_sec < 1.) then prev_sec else sec
					)
					(
						let choose_display_action action display_action =
							match action with
								| Eat 		-> Eat
								| Thunder	-> Thunder
								| Bath		-> Bath
								| Kill		-> Kill
								| Retry | Load -> Waiting
								| _ 		-> display_action
						in choose_display_action action display_action
					)
		in
			try
				Graphic_Interface.init () ; main_loop (Tama.TamaMonad.auto_load ()) (Graphic_Interface.getTime ()) Waiting
			with
			| Graphics.Graphic_failure _ -> prerr_endline "Very very funny. Have a day!\n"
			| _ -> prerr_endline "Something went wrong, plz contact mgrimald or agrumbac and explain exactly WHAT you were doing\nAnyway, have a good day :)"
	end


module Graphics : GRAPHIC_INTERFACE =
	struct
		let getTime () = Unix.gettimeofday ()

		let init () = Graphics.open_graph "" ; Graphics.set_window_title "tamagochiii <3" ; Graphics.auto_synchronize false

		let draw_exit () =
			(
				try
					Graphics.resize_window 1600 1200 ; Graphics.clear_graph () ;
					Graphics.set_color Graphics.black;
					Graphics.fill_rect 0 0 (Graphics.size_x ()) (Graphics.size_y ()) ;
					Graphics.set_color Graphics.white;
					let x = Graphics.size_x ()
						and y = Graphics.size_y ()
					in
					Graphics.moveto (x / 3) (y / 6) ;
					Graphics.draw_string "Made With Salt, Wounds, Salt On Open Wounds and Cries." ;
					Graphics.synchronize ();
					ignore (Graphics.wait_next_event [ Graphics.Button_up ])
				with
					| _ -> ()
			) ;  print_endline "Goodbye. Hope you enjoyed this little game"

		let draw_pika x y pika (action:action) =
			if pika#is_dead then Parse_textures.draw_image Parse_textures.dead   (x / 7) (y * 2 / 7)
			else match action with
				| Eat 		 -> Parse_textures.draw_image Parse_textures.eat     (x / 3) (y / 4 + 25)
				| Thunder 	 -> Parse_textures.draw_image Parse_textures.thunder (x / 3) (y / 4)
				| Bath		 -> Parse_textures.draw_image Parse_textures.bath    (x / 3) (y / 4 + 25)
				| Kill		 -> Parse_textures.draw_image Parse_textures.kill    (x / 3) (y / 4 + 25)
				| _ 		 -> Parse_textures.draw_image Parse_textures.hello   (x / 3) (y / 4 + 25)


		let draw_hud x y health energy hygiene happiness  =
			Graphics.moveto (x / 7) ((5 * y) / 6) ;
			Graphics.draw_string ("health: " ^ (string_of_int health)) ;

			Graphics.draw_rect (x / 7) ((6 * y) / 7) (x / 21) (y / 8);
			if (health < 25) then Graphics.set_color Graphics.red ;
			Graphics.fill_rect (x / 7) ((6 * y) / 7) (x / 21) ((y * health) / (8 * 100));
			Graphics.set_color Graphics.white ;

			Graphics.moveto (2*x / 7) (y - (y / 6)) ;
			Graphics.draw_string ("energy: " ^ (string_of_int energy)) ;

			Graphics.draw_rect (2*x / 7) ((6 * y) / 7) (x / 21) ((y * 100) / (8 * 100));
			if (energy < 25) then Graphics.set_color Graphics.red ;
			Graphics.fill_rect (2*x / 7) ((6 * y) / 7) (x / 21) ((y * energy) / (8 * 100));
			Graphics.set_color Graphics.white ;

			Graphics.moveto (3* x / 7) (y - (y / 6)) ;
			Graphics.draw_string ("hygiene: " ^ (string_of_int hygiene)) ;

			Graphics.draw_rect (3*x / 7) ((6 * y) / 7) (x / 21) ((y * 100) / (8 * 100));
			if (hygiene < 25) then Graphics.set_color Graphics.red ;
			Graphics.fill_rect (3*x / 7) ((6 * y) / 7) (x / 21) ((y * hygiene) / (8 * 100));
			Graphics.set_color Graphics.white ;


			Graphics.moveto (5* x / 7) (y - (y / 6)) ;
			Graphics.draw_string ("happiness: " ^ (string_of_int happiness));

			Graphics.draw_rect (5*x / 7) ((6 * y) / 7) (x / 21) ((y) / (8));
			if (happiness < 25) then Graphics.set_color Graphics.red ;
			Graphics.fill_rect (5*x / 7) ((6 * y) / 7) (x / 21) ((y * happiness) / (8 * 100)) ;
			Graphics.set_color Graphics.white

			(* TODO progress bar  *)

		let draw_buttons x y tama =
			let draw_b x y dx dy str =
				Graphics.draw_rect x y dx dy ;
				Graphics.moveto (x + (dx) / 3) (y + (dy /2));
				Graphics.draw_string str
			in
			if (tama#is_dead <> true) then (
				draw_b (x * 4 / 6) (y / 6 + 20) (x / 7) (y / 12) 	"Kill"		;
				draw_b (x * 3 / 6) (y / 6 + 20) (x / 7) (y / 12) 	"Bath"		;
				draw_b (x / 6) (y / 6 + 20) (x / 7) (y / 12) 		"Eat"		;
				draw_b (x * 2 / 6) (y / 6 + 20) (x / 7) (y / 12) 	"Thunder")
			;
			draw_b (x * 3 / 6) (y / 12) (x / 7) (y / 12) 		"Load"		;
			draw_b (x * 4 / 6) (y / 12) (x / 7) (y / 12) 		"Exit"		;
			draw_b (x * 2 / 6) (y / 12) (x / 7) (y / 12) 		"Retry"		;
			draw_b (x / 6) (y / 12) (x / 7) (y / 12) 			"Save"

		let draw action tama =
			Graphics.resize_window 1600 1200 ; Graphics.clear_graph () ;
			Graphics.set_color Graphics.black;
					Graphics.fill_rect 0 0 (Graphics.size_x ()) (Graphics.size_y ()) ;
					Graphics.set_color Graphics.white;
			draw_pika (Graphics.size_x ()) (Graphics.size_y ()) tama action ;
			let (health, energy, hygiene, happiness) = tama#return_data_tuple in
			draw_hud (Graphics.size_x ()) (Graphics.size_y ()) health energy hygiene happiness
			;
			draw_buttons (Graphics.size_x ()) (Graphics.size_y ()) tama
			;
			Graphics.synchronize () ;
			Tama.TamaMonad.return tama

		let get_action () =
			let s = Graphics.wait_next_event [ Graphics.Poll ] in
			if (s.button) then
			begin
				let s = Graphics.wait_next_event [ Graphics.Button_up ] in
				let x = (Graphics.size_x ())
					and y = (Graphics.size_y ())
				in
				let verif vx vy x1 y1 x2 y2 =
					(vx >= x1 && vx <= x1 + x2) && (vy >= y1 && vy <= y1 + y2)
				in
				 match (s.mouse_x, s.mouse_y) with
				| (vx,vy) when verif vx vy (x / 6) (y / 12) (x / 7) (y / 12) 			-> Save
				| (vx,vy) when verif vx vy (x / 6) (y / 6 + 20) (x / 7) (y / 12) 		-> Eat
				| (vx,vy) when verif vx vy (x * 2 / 6) (y / 12) (x / 7) (y / 12) 		-> Retry
				| (vx,vy) when verif vx vy (x * 2 / 6) (y / 6 + 20) (x / 7) (y / 12) 	-> Thunder
				| (vx,vy) when verif vx vy (x * 3 / 6) (y / 12) (x / 7) (y / 12) 		-> Load
				| (vx,vy) when verif vx vy (x * 3 / 6) (y / 6 + 20) (x / 7) (y / 12) 	-> Bath
				| (vx,vy) when verif vx vy (x * 4 / 6) (y / 12) (x / 7) (y / 12) 		-> Exit
				| (vx,vy) when verif vx vy (x * 4 / 6) (y / 6 + 20) (x / 7) (y / 12) 	-> Kill
				| _ -> Waiting
			end
			else
				Waiting

	end


module Shell : GRAPHIC_INTERFACE =
	struct
		let getTime () = Unix.gettimeofday ()

		let init () = ()

		let draw_pika tama action =
			if tama#is_dead then print_endline "\nTAMA IS DED, like, soooo DED\n" else
			(
				print_endline "       ,___          .-;'";
				print_endline "       `\"-.`\\_...._/`.`";
				print_endline "    ,      \\        /";
				print_endline " .-' ',    / ()   ()\\";
				print_endline "`'._   \\  /()    .  (|";
				print_endline "    > .' ;,     -'-  /";
				print_endline "   / <   |;,     __.;";
				print_endline "   '-.'-.|  , \\    , \\";
				print_endline "      `>.|;, \\_)    \\_)";
				print_endline "       `-;     ,    /";
				print_endline "          \\    /   <";
				print_endline "           '. <`'-,_)";
				print_endline "            '._)";

			match action with
				| Eat 		 -> print_endline "[Eat]"
				| Thunder 	 -> print_endline "[Thunder]"
				| Bath		 -> print_endline "[Bath]"
				| Kill		 -> print_endline "[Kill]"
				| _ 		 -> print_endline "[None]"
			)

		let draw action tama =
			let (health, energy, hygiene, happiness) = tama#return_data_tuple in
			print_endline ("health[" ^ (string_of_int health) ^ "]" ^
						"energy[" ^ (string_of_int energy) ^ "]" ^
						"hygiene[" ^ (string_of_int hygiene) ^ "]" ^
						"happiness[" ^ (string_of_int happiness) ^ "]") ;
			draw_pika tama action ;
			Tama.TamaMonad.return tama

		let get_action () =
			try
				print_endline "Available actions : ";
				print_endline "\teat  - bath - kill - thunder";
				print_endline "\texit - save - load - retry";
				match String.lowercase_ascii (
						String.trim (
							read_line ()
						)
					)
				with
				| "eat" 	-> Eat
				| "thunder" -> Thunder
				| "bath"	-> Bath
				| "kill"	-> Kill
				| "save"	-> Save
				| "retry"	-> Retry
				| "load"	-> Load
				| "exit"	-> Exit
				| x 		-> Waiting
			with
			| _ 			-> Waiting

		let draw_exit () = print_endline "Goodbye. Hope you enjoyed"

	end

module Shell_User_Interface : USER_INTERFACE = Make_User_Interface (Shell)
module Graphics_User_Interface : USER_INTERFACE = Make_User_Interface (Graphics)

(*

In this part you must provide sufficient functionnality to demonstrate your almighty power in OCaml.
• Your program must draw a wibbily wobbly timey wimey creature on the main screen
• You must also draw basic meters : health, hygiene, energy, happyness (you’re free to draw it as you like but it must remain consistent with the concept.)
• You must also draw basic button/area to allow the end user to select an action to perform : EAT, THUNDER, BATH, KILL

*)

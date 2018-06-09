(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   view.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: agrumbac <agrumbac@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/06/08 23:07:10 by agrumbac          #+#    #+#             *)
(*   Updated: 2018/06/09 00:02:36 by agrumbac         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

(* this manages the view *)

type action = Eat | Thunder | Bath | Kill | Waiting | Retry | Save | Load | Exit


module type GRAPHIC_INTERFACE =
	sig
		val draw : Tama.pet -> Tama.TamaMonad.t
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
			let bind_tama_t = Tama.TamaMonad.bind (Tama.TamaMonad.bind tama_t (Tama.TamaMonad.decrease_health_by_n delta_seconds)) in  
			match act with
			| Eat 		-> bind_tama_t Tama.TamaMonad.eat
			| Thunder	-> bind_tama_t Tama.TamaMonad.thunder
			| Bath		-> bind_tama_t Tama.TamaMonad.bath
			| Kill		-> bind_tama_t Tama.TamaMonad.kill
			| Save		-> bind_tama_t (Tama.TamaMonad.backup_to "./auto_save")
			| Load		-> bind_tama_t (Tama.TamaMonad.recover_from "./auto_save")
			| _			-> bind_tama_t Tama.TamaMonad.return
		
		let main () = 
			let rec main_loop tama_t prev_sec = 
				let tama_t 	= 
					Tama.TamaMonad.bind tama_t Graphic_Interface.draw 
				in let sec 	= Graphic_Interface.getTime () in 
				let action 	= Graphic_Interface.get_action () 	in
				match action with
				| Exit -> Graphic_Interface.draw_exit ()
				| Retry -> 
					main_loop 
					(
						Tama.TamaMonad.return 
						(
							new Tama.pet 100 100 100 100
						)
					)
					(
						sec
					)
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
		in main_loop (Tama.TamaMonad.return (new Tama.pet 100 100 100 100)) (Graphic_Interface.getTime ())	
	end


module Shell : GRAPHIC_INTERFACE =
	struct
		let getTime () = Unix.gettimeofday ()

		let draw tama = 
			let (health, energy, hygiene, happiness) = tama#return_data_tuple in
			print_string "health[" ; print_int health; print_endline "]" ;
			print_string "energy[" ; print_int energy; print_endline "]" ;
			print_string "hygiene[" ; print_int hygiene; print_endline "]" ;
			print_string "happiness[" ; print_int happiness; print_endline "]" ;
			if tama#is_dead then print_endline "TAMA IS DED, like, soooo DED"
			; Tama.TamaMonad.return tama

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

(*

In this part you must provide sufficient functionnality to demonstrate your almighty power in OCaml.
• Your program must draw a wibbily wobbly timey wimey creature on the main screen
• You must also draw basic meters : health, hygiene, energy, happyness (you’re free to draw it as you like but it must remain consistent with the concept.)
• You must also draw basic button/area to allow the end user to select an action to perform : EAT, THUNDER, BATH, KILL

*)

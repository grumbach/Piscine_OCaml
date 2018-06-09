(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   view_gui.ml                                        :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: agrumbac <agrumbac@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/06/08 23:07:10 by agrumbac          #+#    #+#             *)
(*   Updated: 2018/06/09 21:34:50 by agrumbac         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

(* ============================== Global Constants ========================== *)

let eat_callback = (fun () -> print_endline "EAT button")
let thunder_callback = (fun () -> print_endline "THUNDER button")
let bath_callback = (fun () -> print_endline "BATH button")
let kill_callback = (fun () -> print_endline "KILL button")

(* ============================== Callbacks ================================= *)

let progress_timeout pbar () =
	let new_val =
	print_endline (string_of_float pbar#fraction) ; flush stdout;
	let v = pbar#fraction -. 0.01 in
	if v < 0. then 0.0 else v in
	pbar#set_fraction new_val;
	(* As this is a timeout function, return true so that it
	* continues to get called *)
	true

let progress_eat pbar vbox () =
	let new_val =
	print_endline (string_of_float pbar#fraction) ; flush stdout;
	let v = pbar#fraction +. 0.2 in
	if v > 1. then 1. else v
	in
	pbar#set_fraction new_val

(* ============================== End and Cleanup =========================== *)

let destroy_timer timer () =
	GMain.Timeout.remove timer;
	GMain.Main.quit ()

(* ============================== Init ====================================== *)

let locale = GtkMain.Main.init ()

let init_gui =
	let window = GWindow.window ~border_width:10 ~width:1024 ~height:640 ~title:"Tama" () in
	let vbox = GPack.vbox ~packing:window#add () in
(*
	(* Create a centering alignment object *)
	let align = GBin.alignment ~xalign:0.5 ~yalign:0.5
	~xscale:0.0 ~yscale:0.0 ~packing:vbox#add () in *)

(* ----------------------------- Buttons -------------------------------------*)
	let button_quit = GButton.button ~label:"QUIT" ~packing:vbox#add () in
	ignore(button_quit#connect#clicked ~callback:window#destroy);

	let button_eat = GButton.button ~label:"EAT" ~packing:vbox#add () in
	ignore(button_eat#connect#clicked ~callback:eat_callback);

	let button_thunder = GButton.button ~label:"THUNDER" ~packing:vbox#add () in
	ignore(button_thunder#connect#clicked ~callback:thunder_callback);

	let button_bath = GButton.button ~label:"BATH" ~packing:vbox#add () in
	ignore(button_bath#connect#clicked ~callback:bath_callback);

	let button_kill = GButton.button ~label:"KILL" ~packing:vbox#add () in
	ignore(button_kill#connect#clicked ~callback:kill_callback);

(* ----------------------------- Status Bars ---------------------------------*)
	let bar_health = GRange.progress_bar ~pulse_step:0.01 ~packing:vbox#add () in
	bar_health#set_fraction 1.;

	let bar_hygiene = GRange.progress_bar ~pulse_step:0.01 ~packing:vbox#add () in
	bar_hygiene#set_fraction 1.;

	let bar_happiness = GRange.progress_bar ~pulse_step:0.01 ~packing:vbox#add () in
	bar_happiness#set_fraction 1.;

	let bar_energy = GRange.progress_bar ~pulse_step:0.01 ~packing:vbox#add () in
	bar_energy#set_fraction 0.50;

	(* Add a timer callback to update the value of the progress bar *)
	(* let timer = GMain.Timeout.add ~ms:100 ~callback:(progress_timeout pbar) in
	ignore(GMisc.separator `HORIZONTAL ~packing:vbox#add ()); *)

	let image  =  GMisc.image ~packing:vbox#add () in
	image#set_file "textures/bath.png";

	(* ignore(window#connect#destroy ~callback:(destroy_timer timer)); *)

	window#show ()

(* ============================== Main Loop ================================= *)

let main () =

	init_gui;

	(* ~key:GdkKeysyms._Q ~callback: GMain.Main.quit; *)

	GMain.Main.main ()

let () = main ()

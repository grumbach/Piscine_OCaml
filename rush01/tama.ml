(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   tama.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: agrumbac <agrumbac@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/06/08 23:06:57 by agrumbac          #+#    #+#             *)
(*   Updated: 2018/06/09 15:11:35 by agrumbac         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

(* ========================= Tama Pet ======================================= *)

class pet initial_Health initial_Energy initial_Hygiene initial_Happiness =
	object (self)

		val _Health:int = match initial_Health with
			| x when x > 100	 -> 100
			| x when x < 0 		 -> 0
			| x 				 -> x 
		val _Energy:int = match initial_Energy with
			| x when x > 100	 -> 100
			| x when x < 0 		 -> 0
			| x 				 -> x 

		val _Hygiene:int = match initial_Hygiene with
			| x when x > 100	 -> 100
			| x when x < 0 		 -> 0
			| x 				 -> x 

		val _Happiness:int = match initial_Happiness with
			| x when x > 100	 -> 100
			| x when x < 0 		 -> 0
			| x 				 -> x 


		method eat =
			new pet
				(_Health + 25)
				(_Energy - 10)
				(_Hygiene - 20)
				(_Happiness + 5)

		method thunder =
			new pet
				(_Health - 20)
				(_Energy + 25)
				_Hygiene
				(_Happiness - 20)

		method bath =
			new pet
				(_Health - 20)
				(_Energy - 10)
				(_Hygiene + 25)
				(_Happiness + 5)

		method kill =
			new pet
				(_Health - 20)
				(_Energy - 10)
				_Hygiene
				(_Happiness + 20)

		method decrease_health_by_1 =
			new pet
				(_Health - 1)
				_Energy
				_Hygiene
				_Happiness

		method is_dead :bool =
			(_Health <= 0 || _Energy <= 0 || _Hygiene <= 0 || _Happiness <= 0)

		method return_data_tuple =
			(_Health, _Energy, _Hygiene, _Happiness)

	end

(* ========================= Tama Monad ===================================== *)

module TamaMonad =
struct
	type t = Alive of pet | Dead

	let return x =
		if x#is_dead then Dead else Alive x

	let bind x (f:pet -> t) =
		match x with
		| Alive tama -> f tama
		| Dead -> Dead

(* ------------------------- methods ---------------------------------------- *)

	let eat (tama:pet) :t =
		let new_tama = tama#eat in
		if new_tama#is_dead then Dead else Alive new_tama

	let thunder (tama:pet) :t =
		let new_tama = tama#thunder in
		if new_tama#is_dead then Dead else Alive new_tama

	let bath (tama:pet) :t =
		let new_tama = tama#bath in
		if new_tama#is_dead then Dead else Alive new_tama

	let kill (tama:pet) :t =
		let new_tama = tama#kill in
		if new_tama#is_dead then Dead else Alive new_tama

	let decrease_health_by_1 (tama:pet) :t =
		let new_tama = tama#decrease_health_by_1 in
		if new_tama#is_dead then Dead else Alive new_tama

	let decrease_health_by_n (n:float) (tama:pet)  :t =
		let rec loop tama n = 
			if (n >= 1.)
			then loop tama#decrease_health_by_1 (n -. 1.)
			else tama
		in
		let new_tama = loop tama n in
		if new_tama#is_dead then Dead else Alive new_tama

(* ------------------------- recover_from ----------------------------------- *)

	let recover_from filename (tama:pet) =
		let (hp, en, hy, ha) =
			try
				let istream = open_in filename in
				try
					let line = input_line istream in
						close_in istream;
					let parse_list lst =
						match lst with
						| hp :: en :: hy :: ha :: [] -> (int_of_string hp, int_of_string en, int_of_string hy, int_of_string ha)
						| _ -> failwith "parse_list"
					in parse_list (String.split_on_char ' ' line)
				with
				| _ -> close_in istream; prerr_endline ("[TAMA hates you] failed reading from: " ^ filename) ; (0, 0, 0, 0)
			with
			| _ -> prerr_endline ("[TAMA hates you] failed opening: " ^ filename); (0, 0, 0, 0)
		in
		return (new pet (hp) (en) (hy) (ha))

(* ------------------------- backup_to -------------------------------------- *)

	let backup_to filename (tama:pet) =
		begin
			try
				let ostream = open_out filename in
				try
					let (hp, en, hy, ha) = tama#return_data_tuple in
					Printf.fprintf ostream "%d %d %d %d\n" (hp) (en) (hy) (ha);
					close_out ostream
				with
				| _ -> close_out ostream; prerr_endline ("[TAMA hates you] failed writing to: " ^ filename)
			with
			| _ -> prerr_endline ("[TAMA hates you] failed opening: " ^ filename)
		end ;
		return tama
end

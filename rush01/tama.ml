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

		val _Health:int = initial_Health
		val _Energy:int = initial_Energy
		val _Hygiene:int = initial_Hygiene
		val _Happiness:int = initial_Happiness

		method eat =
		{<
			_Health = _Health + 25;
			_Energy = _Energy - 10;
			_Hygiene = _Hygiene - 20;
			_Happiness = _Happiness + 5
		>}

		method thunder =
		{<
			_Health = _Health - 20;
			_Energy = _Energy + 25;
			_Hygiene = _Hygiene;
			_Happiness = _Happiness - 20
		>}

		method bath =
		{<
			_Health = _Health - 20;
			_Energy = _Energy - 10;
			_Hygiene = _Hygiene + 25;
			_Happiness = _Happiness + 5
		>}

		method kill =
		{<
			_Health = _Health - 20;
			_Energy = _Energy - 10;
			_Hygiene = _Hygiene;
			_Happiness = _Happiness + 20
		>}

		method decrease_health_by_1 =
		{<
			_Health = _Health - 1;
			_Energy = _Energy;
			_Hygiene = _Hygiene;
			_Happiness = _Happiness
		>}

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

(* ------------------------- recover_from ----------------------------------- *)

	let recover_from filename =
		let (hp, en, hy, ha) =
			try
				let istream = open_in filename in
				try
					let line = input_line istream in
						close_in istream;
					let parse_list lst =
						match lst with
						| hp :: en :: hy :: ha :: [] -> (int_of_string hp, int_of_string en, int_of_string hy, int_of_string ha)
						| _ -> (100, 100, 100, 100)
					in parse_list (String.split_on_char ' ' line)
				with
				| _ -> close_in istream; failwith ("[TAMA hates you] failed reading from: " ^ filename)
			with
			| _ -> failwith ("[TAMA hates you] failed opening: " ^ filename)
		in
		return (new pet (hp) (en) (hy) (ha))

(* ------------------------- backup_to -------------------------------------- *)

	let backup_to x filename =
		match x with
		| Dead -> Dead
		| Alive tama ->
			try
				let ostream = open_out filename in
				try
					let (hp, en, hy, ha) = tama#return_data_tuple in
					Printf.fprintf ostream "%d %d %d %d\n" (hp) (en) (hy) (ha);
					close_out ostream;
					Alive tama
				with
				| _ -> close_out ostream; failwith ("[TAMA hates you] failed writing to: " ^ filename)
			with
			| _ -> failwith ("[TAMA hates you] failed opening: " ^ filename)

end

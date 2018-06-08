(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   tama.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: agrumbac <agrumbac@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/06/08 23:06:57 by agrumbac          #+#    #+#             *)
(*   Updated: 2018/06/09 00:02:03 by agrumbac         ###   ########.fr       *)
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
	type 'a t = Alive of pet | Asleep | Dead

	let return x = Alive x

	let bind x (f:'a -> 'b t) =
		match x with
		| Alive tama -> f tama
		| Asleep -> Asleep
		| Dead -> Dead

(* ------------------------- recover_from ----------------------------------- *)

	let recover_from filename =
		let (hp, en, hy, ha) =
			try
				let istream = open_in filename in
				let line = input_line istream in
				(* TODO actually read (hp, en, hy, ha) from line!! *)
					close_in istream;
					(42, 42, 42, 42)
			with
			| _ -> failwith ("[TAMA hates you] failed reading from: " ^ filename)
		in
		return (new pet (hp) (en) (hy) (ha))

(* ------------------------- backup_to -------------------------------------- *)

	let backup_to x filename =
		match x with
		| Asleep -> Asleep
		| Dead -> Dead
		| Alive tama ->
			try
				let ostream = open_out filename
				and (hp, en, hy, ha) = tama#return_data_tuple in
				Printf.fprintf ostream "%d %d %d %d\n" (hp) (en) (hy) (ha);
				close_out ostream;
				Asleep
			with
			| _ -> failwith ("[TAMA hates you] failed saving to: " ^ filename)

end

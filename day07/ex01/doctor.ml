(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   doctor.ml                                          :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: agrumbac <agrumbac@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/06/06 16:46:05 by agrumbac          #+#    #+#             *)
(*   Updated: 2018/06/06 17:27:37 by agrumbac         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

class doctor (name:string) (age:int) =
	object
		initializer print_endline "The doctor has been created!"

		val _name:string = name
		val mutable _age:int = age
		val _sidekick:People.people = new People.people ("sidekick")
		val mutable _hp:int = 100

		method to_string = _name ^ " has " ^ (string_of_int _hp) ^ "hp, is " ^ (string_of_int _age) ^ " years old, and has a bud : " ^ (_sidekick#to_string)

		method talk = print_endline ("Hi! I’m the Doctor!")

		method travel_in_time (start:int) (arrival:int) =
			let age_change = _age + (arrival - start) in
			_age <- (if age_change > 0 then age_change else 0);
            print_string ("        ___\n" ^
                          "_______(_@_)_______\n" ^
                          "| POLICE      BOX |\n" ^
                          "|_________________|\n" ^
                          " | _____ | _____ |\n" ^
                          " | |###| | |###| |\n" ^
                          " | |###| | |###| |\n" ^
                          " | _____ | _____ |\n" ^
                          " | || || | || || |\n" ^
                          " | ||_|| | ||_|| |\n" ^
                          " | _____ |$_____ |\n" ^
                          " | || || | || || |\n" ^
                          " | ||_|| | ||_|| |\n" ^
                          " | _____ | _____ |\n" ^
                          " | || || | || || |\n" ^
                          " | ||_|| | ||_|| |\n" ^
                          " |       |       |\n" ^
                          " *****************\n")

		method use_sonic_screwdriver = print_endline "Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii"

		method private regenerate = _hp <- 100
	end

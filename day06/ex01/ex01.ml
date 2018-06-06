(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ex01.ml                                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: agrumbac <agrumbac@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2018/06/05 18:52:28 by agrumbac          #+#    #+#             *)
(*   Updated: 2018/06/05 21:10:58 by agrumbac         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

module StringHash =
	struct
		type t = string
		let equal (i:string) (j:string) = i=j
		let hash i = Hashtbl.hash i
	end

module StringHashtbl = Hashtbl.Make(StringHash)


let () =
	let ht = StringHashtbl.create 5 in
	let values = [ "Hello"; "world"; "42"; "Ocaml"; "H" ] in
	let pairs = List.map (fun s -> (s, String.length s)) values in
	List.iter (fun (k,v) -> StringHashtbl.add ht k v) pairs;
	StringHashtbl.iter (fun k v -> Printf.printf "k = \"%s\", v = %d\n" k v) ht

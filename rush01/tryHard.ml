let lire_image_ppm name =
  try
  let fd = open_in_bin name in
   let first_line = input_line fd
   in
   let (width,height) =
       let rec loop line =
        if (line.[0] = '#')
          then loop (input_line fd)
          else line
       in
       match String.split_on_char ' ' ( loop (input_line fd) ) with
       | head::second::[] -> (int_of_string head , int_of_string second)
       | _ -> (*failwith ("couldn't parse line") ; *) (0, 0)

   (* lecture de la ligne contenant 255 *)
   and _ = input_line fd
   in
     let img = Array.make_matrix height width (Graphics.rgb 0 0 0)
        and bool_color = (first_line = "P6")
     in
    let rec loop_i i =
      if (height - 1 >= i) then
        let rec loop_j j =
          if (width - 1 >= j) then
          begin
            begin
              img.(i).(j) <- 
                if bool_color then
                  let x = input_byte fd
                    and y = input_byte fd
                    and z = input_byte fd
                  in Graphics.rgb x y z
                else
                  let x = input_byte fd 
                  in Graphics.rgb x x x
            end
            ;
            loop_j (j+1)
          end
        in begin loop_j 0 ; loop_i (i + 1) end
    in loop_i 0 ;
    close_in fd ;
  img
  with 
  _ -> prerr_endline (name ^ " can not be used")  ; Array.make_matrix 0 0 (Graphics.rgb 0 0 0)

let bath =    lire_image_ppm "./textures/bath.ppm"
let dead =    lire_image_ppm "./textures/dead.ppm"
let eat =     lire_image_ppm "./textures/eat.ppm"
let hello =   lire_image_ppm "./textures/hello.ppm"
let kill =    lire_image_ppm "./textures/kill.ppm"
let thunder = lire_image_ppm "./textures/thunder.ppm"

let draw_image img x y = 
    Graphics.draw_image (Graphics.make_image img) x y

let sample_code () = 
    Graphics.open_graph " 1400x900";
    try 
    let image = (lire_image_ppm "./textures/bath.ppm") in
    draw_image image 0 0 ; ignore(Graphics.read_key ())
    with e -> prerr_endline " image not found"
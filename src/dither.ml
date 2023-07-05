open Core

(* This should look familiar by now! *)
let transform image = 
  Image.foldi image ~init:image ~f:(fun ~x ~y image (r,_g,_b) ->
    let max = Image.max_val image in 
    let value = (Int.to_float r)/.(Int.to_float max) in
    
    let error = if value > 0.5
       then (Image.set image ~x ~y (max,max,max);
    (r - max)) 
    else ((Image.set image ~x ~y (0,0,0); r)) in

    (* if x+1 < Image.width image then let new_val = (value + (7/16)*error)*max in Image.set image ~x:x+1 ~y (new_val,new_val,new_val) in 
    if (x-1 > 0 && y+1 <Image.height image) then let new_val = ((value + 3/16*error))*max in Image.set image ~x:x-1 ~y:y+1 (new_val,new_val,new_val) in 
    if (x+1 < max && y+1 <Image.height image) then let new_val = ((value + 5/16*error))*max in Image.set image ~x:x+1 ~y (new_val,new_val,new_val) in 
    if y+1 < Image.height image then let new_val = ((value + 1/16*error))*max in Image.set image ~x:x+1 ~y (new_val,new_val,new_val) in 
    new_pixel  *)
  )
;;
let command =
  Command.basic
    ~summary:"Dither an image"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      in
      fun () ->
        let image = Image.load_ppm ~filename |> transform in
        Image.save_ppm
          image
          ~filename:(String.chop_suffix_exn filename ~suffix:".ppm" ^ "_dither.ppm")]
;;

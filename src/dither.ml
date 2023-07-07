open Core

let dither_helper
  ~image
  ~x
  ~y
  ~error:(r_error, g_error, b_error)
  ~percent
  ~max
  =
  match Image.get image ~x ~y with
  | exception _ -> ()
  | _ ->
    let curr_val = Image.get image ~x ~y in
    let value =
      ( Pixel.red curr_val
        + Float.to_int (r_error *. percent *. Int.to_float max)
      , Pixel.green curr_val
        + Float.to_int (g_error *. percent *. Int.to_float max)
      , Pixel.blue curr_val
        + Float.to_int (b_error *. percent *. Int.to_float max) )
    in
    Image.set image ~x ~y value
;;

(* This should look familiar by now! *)
let transform image =
  Image.foldi image ~init:image ~f:(fun ~x ~y image (r, g, b) ->
    let max = Image.max_val image in
    let r_value = Int.to_float r /. Int.to_float max in
    let g_value = Int.to_float g /. Int.to_float max in
    let b_value = Int.to_float b /. Int.to_float max in
    let r_error =
      if Float.compare r_value 0.5 > 0
      then (
        Image.set image ~x ~y (max, g, b);
        r_value -. 1.0)
      else (
        Image.set image ~x ~y (0, g, b);
        r_value)
    in
    let g_error =
      if Float.compare g_value 0.5 > 0
      then (
        Image.set image ~x ~y (r, max, b);
        g_value -. 1.0)
      else (
        Image.set image ~x ~y (r, 0, b);
        g_value)
    in
    let b_error =
      if Float.compare b_value 0.5 > 0
      then (
        Image.set image ~x ~y (r, g, max);
        b_value -. 1.0)
      else (
        Image.set image ~x ~y (r, g, 0);
        b_value)
    in
    dither_helper
      ~image
      ~x:(x + 1)
      ~y
      ~error:(r_error, g_error, b_error)
      ~percent:(7 // 16)
      ~max;
    dither_helper
      ~image
      ~x:(x - 1)
      ~y:(y + 1)
      ~error:(r_error, g_error, b_error)
      ~percent:(3 // 16)
      ~max;
    dither_helper
      ~image
      ~x
      ~y:(y + 1)
      ~error:(r_error, g_error, b_error)
      ~percent:(5 // 16)
      ~max;
    dither_helper
      ~image
      ~x:(x + 1)
      ~y:(y + 1)
      ~error:(r_error, g_error, b_error)
      ~percent:(1 // 16)
      ~max;
    image)
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
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_dither.ppm")]
;;

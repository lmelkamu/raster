open! Core

let transform image =
  Image.map image ~f:(fun (r, g, b) ->
    let max_val = Image.max_val image in
    let threshold = Float.to_int (Int.to_float max_val *. 0.7) in
    let r = if r > threshold then max_val - r else r in
    let g = if g > threshold then max_val - g else g in
    let b = if b > threshold then max_val - b else b in
    r, b, g)
;;

let command =
  Command.basic
    ~summary:"Solarize"
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
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_solarize.ppm")]
;;

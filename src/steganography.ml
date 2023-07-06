open! Core

let transform image =
  Image.map image ~f:(fun (r, g, b) ->
    let new_r = r % 4 * 64 in
    let new_g = g % 4 * 64 in
    let new_b = b % 4 * 64 in
    new_r, new_b, new_g)
;;

let command =
  Command.basic
    ~summary:"Hide an image"
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
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "mystery.ppm")]
;;

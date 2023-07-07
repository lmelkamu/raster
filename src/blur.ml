open Core

(* You need to modify this function to blur the input image based on the
   provided radius instead of ignoring it. *)
let transform image ~radius =
  Image.mapi image ~f:(fun ~x ~y _pixel ->
    let x_start = Int.max (x - radius) 0 in
    let x_end = Int.min (x + radius) (Image.width image) in
    let y_start = Int.max (y - radius) 0 in
    let y_end = Int.min (y + radius) (Image.height image) in
    let section = Image.slice image ~x_start ~x_end ~y_start ~y_end in
    Image.mean_pixel section)
;;

let command =
  Command.basic
    ~summary:"Blur an image"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      and radius =
        flag
          "radius"
          (required Command.Param.int)
          ~doc:"N the radius to use when blurring (higher = more blurred)"
      in
      fun () ->
        let image = Image.load_ppm ~filename in
        let image' = transform image ~radius in
        Image.save_ppm
          image'
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_blur.ppm")]
;;

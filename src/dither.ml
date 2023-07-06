open Core

(* This should look familiar by now! *)
let transform image =
  let gray_image = Grayscale.transform image in
  Image.foldi
    gray_image
    ~init:gray_image
    ~f:(fun ~x ~y gray_image (r, _g, _b) ->
    let max = Image.max_val gray_image in
    let value = Int.to_float r /. Int.to_float max in
    let error =
      if Float.compare value 0.5 > 0
      then (
        Image.set gray_image ~x ~y (max, max, max);
        value -. 1.0)
      else (
        Image.set gray_image ~x ~y (0, 0, 0);
        value)
    in
    let () =
      match Image.get gray_image ~x:(x + 1) ~y with
      | exception _ -> ()
      | _ ->
        let curr_val = Image.get gray_image ~x:(x + 1) ~y in
        let right_val =
          Pixel.blue curr_val
          + Float.to_int (+.(7.0 /. 16.0 *. error *. Int.to_float max))
        in
        Image.set gray_image ~x:(x + 1) ~y (right_val, right_val, right_val)
    in
    let () =
      match Image.get gray_image ~x:(x - 1) ~y:(y + 1) with
      | exception _ -> ()
      | _ ->
        let curr_val = Image.get gray_image ~x:(x - 1) ~y:(y + 1) in
        let dl_val =
          Pixel.blue curr_val
          + Float.to_int (value +. (3.0 /. 16.0 *. error *. Int.to_float max))
        in
        Image.set gray_image ~x:(x - 1) ~y:(y + 1) (dl_val, dl_val, dl_val)
    in
    let () =
      match Image.get gray_image ~x ~y:(y + 1) with
      | exception _ -> ()
      | _ ->
        let curr_val = Image.get gray_image ~x ~y:(y + 1) in
        let down_val =
          Pixel.blue curr_val
          + Float.to_int (value +. (5.0 /. 16.0 *. error *. Int.to_float max))
        in
        Image.set gray_image ~x ~y:(y + 1) (down_val, down_val, down_val)
    in
    let () =
      match Image.get gray_image ~x:(x + 1) ~y:(y + 1) with
      | exception _ -> ()
      | _ ->
        let curr_val = Image.get gray_image ~x:(x + 1) ~y:(y + 1) in
        let dr_val =
          Pixel.blue curr_val
          + Float.to_int (value +. (1.0 /. 16.0 *. error *. Int.to_float max))
        in
        Image.set gray_image ~x:(x + 1) ~y:(y + 1) (dr_val, dr_val, dr_val)
    in
    gray_image)
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

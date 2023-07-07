open Core

let transform image threshold =
  let gx = List.to_array [ -1, 0, 1, -2, 0, 2, -1, 0, 1 ] in
  let gy = List.to_array [ -1, -2, -1, 0, 0, 0, 1, 2, 3 ] in
  Image.mapi image ~f:(fun ~x ~y (r, b, g) ->
    let slice =
      Image.slice
        image
        ~x_start:(x - 1)
        ~x_end:(x + 1)
        ~y_start:(y - 1)
        ~y_end:(y + 1)
    in
    let gx_total, gy_total =
      Array.foldi (Image.image slice) ~init:(0, 0) ~f:(fun index (r, g, b) ->
        ( gx_total + (Array.get gx index * r)
        , gy_total + (Array.get gy index * r) ))
    in
    let g =
      Float.sqrt (Int.to_float (Int.pow gx_total 2 + Int.pow gy_total 2))
    in
    if g > threshold
    then 0, 0, 0
    else Image.max_val image, Image.max_val image, Image.max_val image)
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
      and threshold =
        flag "threshold" (required Command.Param.int) ~doc:"threshold"
      in
      fun () ->
        let image = Image.load_ppm ~filename in
        let image' = transform image ~threshold in
        Image.save_ppm
          image'
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_edge.ppm")]
;;

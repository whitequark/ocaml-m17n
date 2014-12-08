let () =
  let filename = Sys.argv.(1) in
  let chan = open_in filename in
  let input =
    let buf = Bytes.create 4096 in
    fun () ->
      if input chan buf 0 (Bytes.length buf) = 0 then None
      else Some (Bytes.unsafe_to_string buf)
  in
  let _lexbuf = Sedlexing_uutf.create ~filename input in
  ()

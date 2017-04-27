open Lwt.Infix

let section = Lwt_log.Section.make "youtube-dl"

let handle_youtube_dl process =
  let open Yojson.Safe.Util in
  try%lwt
    Lwt_io.read_line process#stdout
    >>= fun line ->
    let json = Yojson.Safe.from_string line in
    Lwt.return @@ Ok (
      json |> member "title" |> to_string,
      json |> member "url" |> to_string)
  with
  | End_of_file -> Lwt.return_error "youtube-dl failed"


let fetch url =
  let command = ("" , [|"youtube-dl"; "--dump-single-json"; url|]) in
  Lwt_process.with_process_in command handle_youtube_dl

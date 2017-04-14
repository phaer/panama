open Lwt.Infix
open Websocket_cohttp_lwt
module Player = Panama_player

let section = Lwt_log.Section.make "web"

let strip_prefix_char c s =
  if s.[0] == c
  then String.sub s 1 @@ String.length s - 1
  else s

let request_path_to_filename p =
  print_endline p;
  if p = "/"
  then "index.html"
  else strip_prefix_char '/' p

let kilobytes i = Int64.mul i 1024L
let megabytes i = kilobytes @@ kilobytes i
let read_crunch_file filename =
  Panama_client.read () filename 0L (megabytes 1L)
  >>= function
  | Error (`Unknown_key e) -> Lwt.return_none
  | Ok buffers -> Lwt.return @@ Some (Cstruct.copyv buffers)

module Client = struct
  let next_id = ref 0
  let clients = Hashtbl.create 25
  let count () = Hashtbl.length clients
  let unregister id =
    Hashtbl.remove clients id
  let register id fn =
    unregister id;
    Hashtbl.add clients id fn

  let broadcast message =
    let content = Yojson.Safe.to_string message in
    let frame = Frame.(create ~content ()) in
    Lwt_log.ign_info_f ~section "broadcasting: %s" content;
    Hashtbl.iter
      (fun id send -> send (Some frame))
      clients
end

let rec handle_outgoing outgoing () =
    Lwt_stream.next outgoing
    >>= fun (json) ->
    Lwt.return @@ Client.broadcast json
    >>= handle_outgoing outgoing


let handle_incoming_message push_incoming message =
  push_incoming @@
  try
    Player.Action.of_maybe_yojson @@ Yojson.Safe.from_string message
  with
  | Yojson.Json_error error ->
    Lwt_log.ign_debug_f ~section "received invalid json: %s\n%s" message error;
    None


let handle_incoming_messages client_id push_incoming frame =
  let open Frame in
  Lwt_log.ign_debug_f ~section "%d <- %s" client_id (show frame);
  match frame.opcode with
  | Opcode.Text ->
    handle_incoming_message push_incoming frame.content
  | Opcode.Ping
  | Opcode.Pong -> ()
  | Opcode.Close ->
    Lwt_log.ign_info_f ~section "Client %d sent a close frame" client_id;
    Client.unregister client_id
  | _ ->
    Lwt_log.ign_info_f ~section "Client %d encountered an unknown frame" client_id;
    Client.unregister client_id


let handle_websocket outgoing push_incoming conn req body =
  let client_id = !(Client.next_id) in
  Cohttp_lwt_body.drain_body body
  >>= fun () ->
  Websocket_cohttp_lwt.upgrade_connection
    req (fst conn)
    (handle_incoming_messages client_id push_incoming)
  >>= fun (resp, body, send) ->
  Client.register client_id send;
  Lwt.async (handle_outgoing outgoing);
  Lwt.return (resp, (body :> Cohttp_lwt_body.t))

let handle_http_file path conn req body =
    read_crunch_file @@ request_path_to_filename path
    >>= function
    | Some body ->
      Cohttp_lwt_unix.Server.respond_string
        ~status:`OK ~body ()
    | None ->
      Cohttp_lwt_unix.Server.respond_string
        ~status:`Not_found ~body:"404" ()

let handle_request outgoing push_incoming conn req body =
  let uri_path = Cohttp.Request.uri req |> Uri.path in
  let handler =
    match uri_path with
    | "/socket" -> handle_websocket outgoing push_incoming
    | _ -> handle_http_file uri_path
  in handler conn req body

let connection_closed (ch, _) =
  Lwt_log.ign_info ~section "client closed connection."

let start (host, port) =
  let mode = `TCP (`Port port) in

  let incoming, push_incoming = Lwt_stream.create () in
  let push_incoming e = match e with
  | None -> ()
  | event -> push_incoming event
  in
  let outgoing, push_outgoing = Lwt_stream.create () in

  let server = Cohttp_lwt_unix.Server.make
      ~callback:(handle_request outgoing push_incoming)
      ~conn_closed:connection_closed
      ()
  in

  Lwt.async(fun () -> Cohttp_lwt_unix.Server.create ~mode server);


  Lwt_log.ign_info_f ~section "listening to %s:%d." host port;
  (incoming, push_outgoing)


(* This file is Free Software. See file "LICENSE.md" for more details. *)

(** {3 Panama.Web}
    This module handles connections over websockets.
    Web.start returns a stream and a callback, incoming and push_outgoing.
    incoming - receive Events to update panamas state.
    push_outgoing - push new States to update the client.
*)
open Lwt.Infix


let section = Lwt_log.Section.make "web"



module Client = struct
  type t = {
    id: int;
    client: Websocket_lwt.Connected_client.t;
  }
  let next_id = ref 0
  let clients = Hashtbl.create 25
  let count () = Hashtbl.length clients
  let unregister t =
    Hashtbl.remove clients t.id
  let register t =
    unregister t;
    Hashtbl.add clients t.id t;
    t
  let make client =
    let id = !next_id in
    register { id = id;
               client = client;
             }

  let send t = Websocket_lwt.Connected_client.send t.client
  let recv t = Websocket_lwt.Connected_client.recv t.client
  let broadcast message =
    let content = Yojson.Safe.to_string message in
    let frame = Websocket_lwt.Frame.(create ~content ()) in
    Lwt_log.ign_info_f ~section "broadcasting: %s" content;
    Hashtbl.iter
      (fun id t ->
         Lwt.async
           (fun () -> send t frame))
      clients
end


let handle_message push client message =
  Lwt.return @@ push @@
  try
    Panama_player.Action.of_maybe_yojson @@ Yojson.Safe.from_string message
  with
  | Yojson.Json_error error ->
    Lwt_log.ign_debug_f ~section "received invalid json: %s\n%s" message error;
    None


let rec handle_messages push client () =
  let open Websocket_lwt.Frame in
  let id = client.Client.id in
  let react frame =
    Lwt_log.debug_f ~section "%d <- %s" id (show frame) >>= fun () ->
    match frame.opcode with
    | Opcode.Text ->
      handle_message push client frame.content
    | Opcode.Ping ->
      Client.send client
      @@ create ~opcode:Opcode.Pong ~content:frame.content ()
    | Opcode.Pong ->
      Lwt.return_unit
    | Opcode.Close ->
      Lwt_log.info_f ~section "Client %d sent a close frame" id
      >>= fun () -> Client.send client @@ close 1000
      >>= fun () -> Client.unregister client;
      Lwt.fail Exit
    | _ ->
      Lwt_log.info_f ~section "Client %d encountered an unknown frame" id
      >>= fun () -> Client.send client @@ close 1002
      >>= fun () -> Client.unregister client;
      Lwt.fail Exit
  in
  Client.recv client
  >>= react
  >>= handle_messages push client


let handle_connection push client =
  let client = Client.make client in
  let id = client.Client.id in

  Lwt_log.ign_info_f ~section "New connection (#%d of %d)" id @@ Client.count ();

  try%lwt
    handle_messages push client ()
  with
  | Exit ->
      Lwt_log.info_f ~section "Client %d closed connection" id
  | exn ->
      Lwt_log.info_f ~section "Connection to client %d lost" id >>= fun () ->
      Lwt.fail exn


let rec handle_outgoing outgoing () =
    Lwt_stream.next outgoing
    >>= fun (json) ->
    Lwt.return @@ Client.broadcast json
    >>= handle_outgoing outgoing


let check_request request = true


let listen ctx address handle_connection () =
    Conduit_lwt_unix.endp_to_server ~ctx address
    >>= fun mode ->
    Websocket_lwt.establish_server ~check_request ~ctx ~mode handle_connection


let start (host, port) =
  let address = `TCP (Ipaddr.of_string_exn host, port) in
  let ctx = Conduit_lwt_unix.default_ctx in

  let incoming, push_incoming = Lwt_stream.create () in
  let push_incoming e = match e with
  | None -> ()
  | event -> push_incoming event
  in

  let outgoing, push_outgoing = Lwt_stream.create () in
  Lwt.async (handle_outgoing outgoing);
  Lwt.async (listen ctx address @@ handle_connection push_incoming);

  Lwt_log.ign_info_f ~section "listening to %s:%d." host port;
  (incoming, push_outgoing)

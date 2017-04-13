(* This file is Free Software. See file "LICENSE.md" for more details. *)

(** {4 Panama.Mpv}
    This module handles connections to mpv.
    Mpv.start returns two streams, incoming and outgoing.
    incoming - Events to update panamas player state.
    outgoing - Commands to update the client.
*)
open Lwt.Infix


let section = Lwt_log.Section.make "mpv"


module Command = struct
  type t
    = ObserveProperty of int * Panama_player.Property.t [@name "observe_property"]
    | GetProperty of Panama_player.Property.t [@name "get_property"]
    | SetProperty of Panama_player.Property.t [@name "set_property"]
    | LoadFile of string * string [@name "loadfile"]
    | ShowText of string [@name "show_text"]
    | PlaylistRemove of int [@name "playlist_remove"]
    | Noop
  [@@deriving show, yojson]

  let next_id = ref 0
  let to_yojson t =
    (* TODO fix this ugly hack which is currently
    needed, because {get,observe}_property do not
    take a second value. Optional type parameters
    for properties would be even nicer. But it
    works for now.
    *)
    match to_yojson t with
    | `List [`String "get_property" ; `List l] ->
        `List [`String "get_property" ; List.hd l]
    | `List [`String "observe_property" ; i; `List l] ->
        `List [`String "observe_property" ; i; List.hd l]
    | `List [x ; `List l] -> `List (x :: l)
    | `List [x ; y ; `List l] -> `List (x :: y :: l)
    | v -> v

  let to_payload command =
    let json = Yojson.Safe.to_string
        (`Assoc [("command", to_yojson command);
                 ("request_id", `Int !next_id)])
    in
    incr next_id;
    (* Hashtbl.add pending_requests id promise; *)
    json
end


let rec handle_outgoing output_channel outgoing () =
    Lwt_stream.next outgoing
    >>= fun (command) ->
    let payload = Command.to_payload command in
    Lwt_log.ign_debug_f ~section "sending: %s" payload;
    Lwt_io.write_line output_channel payload
    >>= fun () -> Lwt_io.flush output_channel
    >>= handle_outgoing output_channel outgoing


let listen input_channel push () =
  let rec loop () =
    Lwt_io.read_line input_channel
    >>= fun (message) ->
    (match Panama_player.Action.of_mpv_yojson @@ Yojson.Safe.from_string message with
    | Ok action -> Lwt.return @@ push @@ Some action
    | Error error ->
      Lwt_log.info_f ~section "received unhandled json: %s %s" message)
    >>= loop
  in
  loop ()


let start (socket_path) =
  let address = Lwt_unix.ADDR_UNIX socket_path in

  let incoming, push_incoming = Lwt_stream.create () in
  let push_incoming a = match a with
  | None -> ()
  | action -> push_incoming action
  in

  let outgoing, push_outgoing = Lwt_stream.create() in
  let push_outgoing c = match c with
    | Command.Noop -> ()
    | command -> push_outgoing @@ Some command
  in

  Lwt.async (
    fun () -> Lwt_io.open_connection address
      >|= fun (input_channel, output_channel) ->
      Lwt.async (listen input_channel push_incoming);
      Lwt.async (handle_outgoing output_channel outgoing);
  );

  let properties_to_observe = [
    (Panama_player.Property.Pause false);
    (Panama_player.Property.Position 0);
    (Panama_player.Property.Volume 0);
    (Panama_player.Property.Playlist []);
  ]
  in
  List.iteri (fun i p -> push_outgoing @@ Command.ObserveProperty (i, p)) properties_to_observe;

  Lwt_log.ign_info_f ~section "listening to %s" socket_path;
  (incoming, push_outgoing)

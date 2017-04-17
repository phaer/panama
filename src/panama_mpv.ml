(* This file is Free Software. See file "LICENSE.md" for more details. *)

(** {4 Panama.Mpv}
    This module handles connections to mpv.
    Mpv.start returns two streams, incoming and outgoing.
    incoming - Events to update panamas player state.
    outgoing - Commands to update the client.
*)
open Lwt.Infix
module Player = Panama_player


let section = Lwt_log.Section.make "mpv"
let pending_requests = Hashtbl.create 100

exception Mpv_error of string

module Command = struct
  type t
    = ObserveProperty of int * Player.Property.t [@name "observe_property"]
    | GetProperty of Player.Property.t [@name "get_property"]
    | SetProperty of Player.Property.t [@name "set_property"]
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
    let id = !next_id in
    let json = Yojson.Safe.to_string
        (`Assoc [("command", to_yojson command);
                 ("request_id", `Int id)])
    in
    incr next_id;
    (id, json)

end

let execute push = function
  | Command.Noop ->
    Lwt.return `Null
  | command ->
    let promise = Lwt_mvar.create_empty () in
    let id, payload = Command.to_payload command in
    Hashtbl.add pending_requests id promise;
    push @@ Some payload;
    Lwt_mvar.take promise

let execute_async push = function
  | Command.Noop ->
    ()
  | command ->
    let _, payload = Command.to_payload command in
    push @@ Some payload


let rec handle_outgoing output_channel outgoing () =
    Lwt_stream.next outgoing
    >>= fun (payload) ->
    Lwt_log.debug_f ~section "sending: %s" payload
    >>= fun () -> Lwt_io.write_line output_channel payload
    >>= fun () -> Lwt_io.flush output_channel
    >>= handle_outgoing output_channel outgoing


let listen input_channel push () =
  let open Yojson.Safe.Util in
  let rec loop () =
    Lwt_io.read_line input_channel
    >>= fun (message) ->
    Lwt_log.ign_info_f ~section "received: %s" message;
    let json =
      try
        Yojson.Safe.from_string @@ message
      with exn ->
        raise (Mpv_error (Printexc.to_string exn))
    in
    let data = json |> member "data" in
    let id_option = json |> member "request_id" |> to_int_option in

    (match json |> member "error" |> to_string_option with
    | None -> ()
    | Some "success" -> ()
    | Some error -> raise @@ Mpv_error error);

    (match Player.Action.of_mpv_yojson json with
    | Ok action -> Lwt.return @@ push @@ Some action
    | Error error -> Lwt.return_unit)

    >>= fun () ->
    (match id_option with
      | Some id ->
        if Hashtbl.mem pending_requests id
        then Lwt_mvar.put (Hashtbl.find pending_requests id) data
        else Lwt_log.error_f ~section "response for unknown request: %d" id
      | None -> Lwt.return_unit)
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

  let properties_to_observe = [
    (Player.Property.Pause false);
    (Player.Property.Position 0);
    (Player.Property.Volume 0);
    (Player.Property.Playlist []);
  ]
  in
  List.iteri (fun i p -> execute_async push_outgoing (Command.ObserveProperty (i, p))) properties_to_observe;

  Lwt.async (
    fun () ->
      try%lwt
        Lwt_io.open_connection address
        >|= fun (input_channel, output_channel) ->
        Lwt.async (listen input_channel push_incoming);
        Lwt.async (handle_outgoing output_channel outgoing);
      with exn ->
        raise (Mpv_error (Printexc.to_string exn))
  );
  Lwt_log.ign_info_f ~section "listening to %s" socket_path;

 (incoming, push_outgoing)

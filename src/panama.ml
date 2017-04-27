(* This file is Free Software. See file "LICENSE.md" for more details. *)

(** {1 Panama}
    This module loads the configuration, sets up the environment,
    and starts a new Instance of Panama.App.

    {e %%VERSION%% - {{:%%PKG_HOMEPAGE%% }homepage}}.
*)
open Lwt.Infix
module Store = Panama_store
module Player = Panama_player
module Mpv = Panama_mpv
module Web = Panama_web


let _ = Lwt_log.(add_rule "*" Debug)
let section = Lwt_log.Section.make "main"

let web_address = ("127.0.0.1", 3000)
(** (ip, port) of the websocket, we should listen to. *)

let mpv_address = "/tmp/mpv.socket"
(** path of mpvs ipc socket *)


let resolve_url web source_url () =
    try
      Panama_youtube_dl.fetch source_url
      >>= function
      | Ok (title, media_url) ->
        Lwt.return @@ web.Web.push_incoming
        @@ Some (Player.Action.ItemResolved (source_url, media_url, title));
      | Error e ->
        Lwt_log.debug_f ~section "youtube-dl error: %s" e;
    with exn ->
      Lwt_log.debug_f ~exn ~section "youtube-dl error"


let rec loop web mpv old_state actions () =
  Lwt_stream.next actions
  >>= fun (action) ->
  (* Lwt_log.ign_debug_f ~section "RECV ACTION: %s" @@ Player.Action.show action; *)
  let command, new_state = Store.update old_state action in
  Mpv.execute_async mpv command;

  (match action with
   | Player.Action.PlaylistAdd source_url ->
     Lwt.async @@ resolve_url web source_url;
  | _ -> ());

  Lwt_log.ign_debug_f ~section "NEW_STATE: %s" @@ Store.show new_state;

  let broadcast_state_p =
    new_state <> old_state
    || action == Player.Action.Update
  in
  Lwt.return (
    if broadcast_state_p
    then web.Web.push_outgoing @@ Some (Store.to_yojson @@ new_state)
    else ())
  >>= loop web mpv new_state actions


let start web_address mpv_address =
  let web = Web.start web_address in
  let mpv = Mpv.start mpv_address in
  let actions = Lwt_stream.choose [web.Web.incoming; mpv.Mpv.incoming] in

  web.Web.push_incoming @@ Some (Player.Action.PlaylistAdd "https://www.youtube.com/watch?v=qWG2dsXV5HI");

  let state = Store.{
      playing = false;
      volume = 50;
      position = 0;
      playlist = [];
    }
  in
  loop web mpv state actions ()

let () =
  Lwt_main.run @@
  try%lwt
    start web_address mpv_address
  with exn ->
    Lwt_log.fatal_f ~exn ~section "fatal"

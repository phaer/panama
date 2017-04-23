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


let rec loop web mpv old_state actions () =
  Lwt_stream.next actions
  >>= fun (action) ->
  (* let action_string = Player.Action.show action in *)
  let command, new_state = Store.update old_state action in

  (* Lwt_log.ign_debug_f ~section "action: %s" action_string; *)
  Mpv.execute_async mpv command;


  let broadcast_state_p = new_state <> old_state
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

  Mpv.execute_async mpv @@ Mpv.Command.LoadFile ("https://www.youtube.com/watch?v=10zB1p1nXHg", "append-play");

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

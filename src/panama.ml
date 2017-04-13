(* This file is Free Software. See file "LICENSE.md" for more details. *)

(** {1 Panama}
    This module loads the configuration, sets up the environment,
    and starts a new Instance of Panama.App.

    {e %%VERSION%% - {{:%%PKG_HOMEPAGE%% }homepage}}.
*)

open Lwt.Infix


let _ = Lwt_log.(add_rule "*" Debug)
let section = Lwt_log.Section.make "main"

let web_address = ("127.0.0.1", 3000)
(** (ip, port) of the websocket, we should listen to. *)

let mpv_address = "/tmp/mpv.socket"
(** path of mpvs ipc socket *)

let start web_address mpv_address =
  let web_in, web_push = Panama_web.start web_address in
  let mpv_in, mpv_push = Panama_mpv.start mpv_address in
  let actions = Lwt_stream.choose [web_in; mpv_in] in

  mpv_push @@ Panama_mpv.Command.ObserveProperty (1, (Panama_player.Property.Pause false));
  mpv_push @@ Panama_mpv.Command.LoadFile ("https://www.youtube.com/watch?v=10zB1p1nXHg", "append-play");

  let state = ref (Panama_store.{
      playing = false;
      volume = 50;
      position = 0;
      playlist = [];
    })
  in

  let rec loop () =
    Lwt_stream.next actions
    >>= fun (action) ->
    let action_string = Panama_player.Action.show action in
    let old_state = !state in
    let command, new_state = Panama_store.update old_state action in

    Lwt_log.ign_debug_f ~section "action: %s" action_string;
    state := new_state;
    mpv_push @@ command;

    let broadcast_state_p = new_state <> old_state
                            || action == Panama_player.Action.Update
    in
    Lwt.return (
      if broadcast_state_p
      then web_push @@ Some (Panama_store.to_yojson @@ new_state)
      else ())
    >>= loop
  in
  loop ()

let () =
  Lwt_main.run @@
  try%lwt
  start web_address mpv_address
  with exn ->
    Lwt_log.fatal_f ~exn ~section "fatal"

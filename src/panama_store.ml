(* This file is Free Software. See file "LICENSE.md" for more details. *)

(** {5 Panama.State}
*)
open Lwt.Infix
open Panama_player
open Panama_mpv

type t = {
  playing: bool;
  volume : int;
  position : int;
  playlist : Panama_player.PlaylistItem.t list;
} [@@deriving show, yojson]


let with_property state = function
  | Property.Pause v    -> {state with playing = not v}
  | Property.Volume v   -> {state with volume = v}
  | Property.Position v -> {state with position = v}
  | Property.Playlist v -> {state with playlist = v}
  | _                   -> state


let set_property state property =
  (Command.SetProperty property, with_property state property)


let update state action =
  (match action with
   | Action.TogglePlay ->
     set_property state @@ Property.Pause state.playing
   | Action.Volume vol ->
     set_property state @@ Property.Volume vol
   | Action.Position pos ->
     set_property state @@ Property.Position pos
   | Action.PropertyChange property ->
     (Command.Noop,
     with_property state property)
   | _ ->
     (Command.Noop,
      state))

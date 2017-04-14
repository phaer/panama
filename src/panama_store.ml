(* This file is Free Software. See file "LICENSE.md" for more details. *)

(** {5 Panama.State}
*)
open Lwt.Infix
module Player = Panama_player
module Mpv = Panama_mpv


type t = {
  playing: bool;
  volume : int;
  position : int;
  playlist : Player.PlaylistItem.t list;
} [@@deriving show, yojson]



let with_property state = function
  | Player.Property.Pause v    -> {state with playing = not v}
  | Player.Property.Volume v   -> {state with volume = v}
  | Player.Property.Position v -> {state with position = v}
  | Player.Property.Playlist v -> {state with playlist = List.mapi Player.PlaylistItem.set_index v}
  | _                   -> state


let set_property state property =
  (Mpv.Command.SetProperty property, with_property state property)


let update state action =
  (match action with
   | Player.Action.TogglePlay ->
     set_property state @@ Player.Property.Pause state.playing
   | Player.Action.Volume vol ->
     set_property state @@ Player.Property.Volume vol
   | Player.Action.Position pos ->
     set_property state @@ Player.Property.Position pos
   | Player.Action.PlaylistAdd url ->
     (Mpv.Command.LoadFile (url, "append-play"),
      state)
   | Player.Action.PlaylistRemove index ->
     (Mpv.Command.PlaylistRemove index,
      state)
   | Player.Action.PlaylistSelect index ->
     set_property state @@ Player.Property.PlaylistPosition index
   | Player.Action.PropertyChange property ->
     (Mpv.Command.Noop,
      with_property state property)
   | _ ->
     (Mpv.Command.Noop,
      state))

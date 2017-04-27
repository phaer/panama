(* This file is Free Software. See file "LICENSE.md" for more details. *)

(** {5 Panama.State}
*)
open Lwt.Infix
open Panama_player
module Mpv = Panama_mpv

let section = Lwt_log.Section.make "player"

type t = {
  playing: bool;
  volume : int;
  position : int;
  playlist : Playlist.t;
} [@@deriving show, yojson]



let with_property state = function
  | Property.Pause v    -> {state with playing = not v}
  | Property.Volume v   -> {state with volume = v}
  | Property.Position v -> {state with position = v}
  | Property.Playlist v ->
    {state with playlist = Playlist.of_mpv state.playlist v }

  | Property.PlaylistPosition v ->
    { state with
      playlist = Playlist.with_nth_item
          state.playlist v (PlaylistItem.set_loading true)}
  | Property.Loading v ->
    Lwt_log.ign_debug_f ~section "LOADING: %s" @@ string_of_bool v;
    {state with
     playlist = Playlist.with_current_item
         state.playlist (PlaylistItem.set_loading v)}


let set_property state property =
  (Mpv.Command.SetProperty property, with_property state property)


let update state action =
  (match action with
   | Action.TogglePlay ->
     set_property state @@ Property.Pause state.playing
   | Action.Volume vol ->
     set_property state @@ Property.Volume vol
   | Action.Position pos ->
     set_property state @@ Property.Position pos
   | Action.PlaylistAdd url ->
     (Mpv.Command.LoadFile (url, "append-play"),
      state)
   | Action.PlaylistRemove index ->
     (Mpv.Command.PlaylistRemove index,
      state)
   | Action.PlaylistSelect index ->
     set_property state @@ Property.PlaylistPosition index
   | Action.PropertyChange property ->
     (Mpv.Command.Noop,
      with_property state property)
   | Action.ItemResolved (source_url, media_url, title) ->
     let new_state = { state with
                       playlist = Playlist.with_item_by_filename state.playlist source_url (fun item ->
                           { item with
                             PlaylistItem.media_url = Some media_url;
                             PlaylistItem.title = Some title})}
     in
     (Mpv.Command.Noop, new_state)
   | _ ->
     (Mpv.Command.Noop,
      state))

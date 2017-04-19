(* This file is Free Software. See file "LICENSE.md" for more details. *)

(** {2 Panama.Player}
*)
open Lwt.Infix



let section = Lwt_log.Section.make "player"

module PlaylistItem = struct
  type t = {
    filename: string;
    title: string option [@default None];
    media_url: string option [@default None];
    index : int [@default 0];
    current : bool [@default false];
    playing : bool [@default false];
    loading : bool [@default false];
  } [@@deriving show, yojson]

  let to_yojson t =
    `Assoc [("source_url", `String t.filename);
            ("index", `Int t.index);
            ("current", `Bool t.current);
            ("playing", `Bool t.playing);
            ("loading", `Bool t.loading);
           ]
  let set_loading v item = {item with loading = v}
end

module Playlist = struct
  type t = PlaylistItem.t list
  [@@deriving show, yojson]

  let of_mpv playlist mpv_playlist =
    let playlist_length = List.length playlist in
    List.mapi (fun index mpv_item ->
        if index >= playlist_length
        then mpv_item
        else
          let item = List.nth playlist index in
          PlaylistItem.{ mpv_item with
            title = item.title;
            media_url = item.media_url;
            loading = item.loading;
            index = index;
          })
      mpv_playlist

  let with_current_item playlist fn =
    List.map (fun item ->
        if item.PlaylistItem.current
        then fn item
        else item)
      playlist

  let with_nth_item playlist position fn =
    List.mapi (fun index item ->
        if index == position
        then fn item
        else item)
      playlist
end

module Property = struct
  type t
    = Playlist of Playlist.t [@name "playlist"]
    | PlaylistPosition of int [@name "playlist-pos"]
    | Volume of int [@name "volume"]
    | Position of int [@name "percent-pos"]
    | Pause of bool [@name "pause"]
    | Loading of bool [@name "loading"]
  [@@deriving show, yojson]

  let of_yojson json =
    of_yojson @@ match json with
    | `List [`String "percent-pos"; `Float f] ->
      `List [`String "percent-pos"; `Int (int_of_float f)]
    | `List [`String "volume"; `Float f] ->
        `List [`String "volume"; `Int (int_of_float f)]
    | x -> x
end


module Action = struct
  type t
    = Update [@name "update"]
    | TogglePlay [@name "toggle-play"]
    | Volume of int [@name "volume"]
    | Position of int [@name "position"]
    | PlaylistAdd of string [@name "playlist-add"]
    | PlaylistRemove of int [@name "playlist-remove"]
    | PlaylistSelect of int [@name "playlist-select"]
    | PropertyChange of Property.t [@name "property-change"]
  [@@deriving show, yojson]

  let of_maybe_yojson json =
    match of_yojson json with
    | Ok command -> Some command
    | Error error ->
      raise @@ Yojson.Json_error error

  let of_mpv_event = function
    | `List [`String "playback-restart"] -> to_yojson @@ PropertyChange (Property.Loading false)
    | x -> x


  let of_mpv_yojson json =
    let open Yojson.Safe.Util in
    let event = json |> member "event" in
    let name = json |> member "name" in
    let data = json |> member "data" in
    let args = List.filter (fun i -> i != `Null) [name; data] in
    let json = match args with
    | [] -> `List [event]
    | x  -> `List [event; `List x]
    in
    of_yojson @@ of_mpv_event json
  end

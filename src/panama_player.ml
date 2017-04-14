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
  } [@@deriving show, yojson]

  let to_yojson t =
    `Assoc [("source_url", `String t.filename);
            ("index", `Int t.index);
            ("current", `Bool t.current)]

  let set_index i e = {e with index = i}
end


module Property = struct
  type t
    = Playlist of PlaylistItem.t list [@name "playlist"]
    | PlaylistPosition of int [@name "playlist-pos"]
    | Volume of int [@name "volume"]
    | Position of int [@name "percent-pos"]
    | Pause of bool [@name "pause"]
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

  let of_mpv_yojson json =
    let open Yojson.Safe.Util in
    let event = json |> member "event" in
    let name = json |> member "name" in
    let data = json |> member "data" in
    let args = List.filter (fun i -> i != `Null) [name; data] in
    of_yojson @@ match args with
    | [] -> `List [event]
    | x  -> `List [event; `List x]
end

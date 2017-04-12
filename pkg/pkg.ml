#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "panama" @@ fun c ->
  Ok [Pkg.bin "src/panama"]

From Corelib Require Extraction.
From Stdlib Require Import extraction.ExtrOcamlBasic.
From Stdlib Require Import extraction.ExtrOcamlNatInt.

Require Import CalendarPuzzle.
Require Import GeneratedSolutions.
Require Import AllDatesProof.

Extraction Language OCaml.
Set Extraction Optimize.

(** Build-time smoke test: Rocq extracts and asks OCaml to compile these terms. *)
Extraction TestCompile verify lookup_solution certified_dateb.

(** The generated file is consumed directly by Dune/js_of_ocaml. *)
Extraction "../src/calendar_verified.ml"
  piece_id placement verify
  solution_table lookup_solution certified_dateb.

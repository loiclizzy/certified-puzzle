open Js_of_ocaml

let piece_of_string = function
  | "A" -> Calendar_verified.A
  | "B" -> Calendar_verified.B
  | "C" -> Calendar_verified.C
  | "D" -> Calendar_verified.D
  | "E" -> Calendar_verified.E
  | "F" -> Calendar_verified.F
  | "G" -> Calendar_verified.G
  | "H" -> Calendar_verified.H
  | s -> invalid_arg ("unknown piece: " ^ s)

let int_field obj name =
  Js.Unsafe.get obj name |> Js.Unsafe.coerce |> Js.to_float |> int_of_float

let string_field obj name =
  Js.Unsafe.get obj name |> Js.Unsafe.coerce |> Js.to_string

let placement_of_js obj =
  Calendar_verified.Place
    (piece_of_string (string_field obj "piece"),
     int_field obj "orientation",
     int_field obj "row",
     int_field obj "col")

let verify_js month day cert_js =
  try
    let cert =
      Js.to_array cert_js
      |> Array.to_list
      |> List.map placement_of_js
    in
    Js.bool (Calendar_verified.verify month day cert)
  with _ -> Js._false

let () =
  let api = Js.Unsafe.obj [||] in
  Js.Unsafe.set api "engine" (Js.string "rocq-extracted-ocaml");
  Js.Unsafe.set api "verify" (Js.wrap_callback verify_js);
  Js.Unsafe.set Js.Unsafe.global "CalendarOCamlVerifier" api

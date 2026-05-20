open Js_of_ocaml
open Beluga
open Beluga_syntax.Syncom

type session =
  { state : Command.state
  ; buf : Buffer.t
  ; ppf : Format.formatter
  ; scan_pos : int ref
  }

let ends_with s suffix =
  let ls = String.length s and lf = String.length suffix in
  ls >= lf && String.sub s (ls - lf) lf = suffix

let parse_meta_line line =
  let t = String.trim line in
  if String.length t < 8 || t.[0] <> '#' || t.[1] <> '#' then None
  else
    let rest = String.trim (String.sub t 2 (String.length t - 2)) in
    if ends_with rest " begin:" then
      let phase = String.trim (String.sub rest 0 (String.length rest - 7)) in
      Some (phase, "begin")
    else if ends_with rest " done:" then
      let phase = String.trim (String.sub rest 0 (String.length rest - 6)) in
      Some (phase, "done")
    else if ends_with rest ":" then
      let phase = String.trim (String.sub rest 0 (String.length rest - 1)) in
      Some (phase, "begin")
    else
      None

let report_progress phase state =
  try
    let global = Js.Unsafe.global in
    let cb = Js.Unsafe.get global (Js.string "reportBelugaProgress") in
    if Js.Optdef.test cb then
      let obj =
        object%js
          val phase = Js.string phase
          val state = Js.string state
        end
      in
      ignore (Js.Unsafe.fun_call cb [| Js.Unsafe.inject obj |])
  with _ -> ()

let scan_buffer_for_progress buf scan_pos =
  let contents = Buffer.contents buf in
  let len = String.length contents in
  if len <= !scan_pos then ()
  else (
    let start = !scan_pos in
    scan_pos := len;
    let slice = String.sub contents start (len - start) in
    let lines = String.split_on_char '\n' slice in
    List.iter
      (fun line ->
         match parse_meta_line line with
         | Some (phase, state) -> report_progress phase state
         | None -> ())
      lines)

let make_progress_formatter buf scan_pos =
  let flush () = scan_buffer_for_progress buf scan_pos in
  let out s pos len = Buffer.add_substring buf s pos len in
  Format.make_formatter out flush

let drain session =
  Format.pp_print_flush session.ppf ();
  scan_buffer_for_progress session.buf session.scan_pos;
  let s = Buffer.contents session.buf in
  Buffer.clear session.buf;
  session.scan_pos := 0;
  s

let create () =
  let buf = Buffer.create 4096 in
  let scan_pos = ref 0 in
  let ppf = make_progress_formatter buf scan_pos in
  Error.disable_colored_output ();
  Logic.Options.more_solutions_prompt := (fun () -> false);
  Chatter.level := 1;
  let state = Command.create_initial_state ~ppf () in
  { state; buf; ppf; scan_pos }

let normalize_exn e =
  match Js_error.of_exn e with
  | Some err ->
    (try if Js_error.name err = "RangeError" then Stack_overflow else e
     with _ -> e)
  | None -> e

let load_from_string session content =
  session.scan_pos := 0;
  Buffer.clear session.buf;
  try
    ignore
      (Command.load_from_string session.state ~virtual_filename:"input.bel"
         ~content : Synint.Sgn.sgn);
    (drain session, true)
  with e ->
    let e = normalize_exn e in
    let bt = Printexc.get_backtrace () in
    let msg =
      try Format.asprintf "%t" (Error.find_printer e)
      with _ -> Printexc.to_string e
    in
    Buffer.clear session.buf;
    session.scan_pos := 0;
    (msg ^ "\n" ^ bt, false)

let run_command session input =
  session.scan_pos := 0;
  Buffer.clear session.buf;
  try
    Command.interpret_command session.state ~input;
    drain session
  with e ->
    let e = normalize_exn e in
    let msg =
      try Format.asprintf "%t" (Error.find_printer e)
      with _ -> Printexc.to_string e
    in
    Buffer.clear session.buf;
    session.scan_pos := 0;
    msg

let () =
  Printexc.record_backtrace true;
  let session = ref (create ()) in
  Js.export "Beluga"
    (object%js
       method create =
         session := create ();
         Js.string "Beluga session created."

       method loadFromString content =
         let s = Js.to_string content in
         let trial = create () in
         let (out, ok) = load_from_string trial s in
         if ok then session := trial;
         Js.string out

       method runCommand input =
         let s = Js.to_string input in
         Js.string (run_command !session s)

       method reset =
         session := create ();
         Js.string "Session reset."
    end)

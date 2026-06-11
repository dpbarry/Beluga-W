open Js_of_ocaml
open Beluga
open Beluga_syntax.Syncom

module P = Prettyint.DefaultPrinter

type session =
  { state : Command.state
  ; buf : Buffer.t
  ; ppf : Format.formatter
  ; scan_pos : int ref
  }

let fingerprint_content s =
  let hash = ref 0x811c9dc5l in
  for i = 0 to String.length s - 1 do
    hash := Int32.logxor !hash (Int32.of_int (Char.code s.[i]));
    hash := Int32.mul !hash 0x01000193l
  done;
  Printf.sprintf "%d:%08lx" (String.length s) !hash

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
  Logic.Options.output_formatter := ppf;
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
  Coverage.reset_information ();
  try
    ignore
      (Command.load_from_string session.state ~virtual_filename:"input.bel"
         ~content : Synint.Sgn.sgn);
    let main_out = drain session in
    let warnings = Coverage.get_information () in
    Coverage.reset_information ();
    let combined =
      if String.length warnings = 0 then main_out
      else if String.length main_out = 0 then warnings
      else main_out ^ "\n" ^ warnings
    in
    (combined, true)
  with e ->
    let e = normalize_exn e in
    let bt = Printexc.get_backtrace () in
    let msg =
      try Format.asprintf "%t" (Error.find_printer e)
      with _ -> Printexc.to_string e
    in
    Buffer.clear session.buf;
    session.scan_pos := 0;
    Coverage.reset_information ();
    (msg ^ "\n" ^ bt, false)

(* Like run_command but also reports whether the command ran without raising,
   so JSON wrappers can distinguish a real result from an error message that
   was interpreted and returned in the same channel. *)
let run_command_status session input =
  session.scan_pos := 0;
  Buffer.clear session.buf;
  try
    Command.interpret_command session.state ~input;
    (drain session, true)
  with e ->
    let e = normalize_exn e in
    let msg =
      try Format.asprintf "%t" (Error.find_printer e)
      with _ -> Printexc.to_string e
    in
    Buffer.clear session.buf;
    session.scan_pos := 0;
    (msg, false)

let run_command session input = fst (run_command_status session input)

(* ----- IDE JSON helpers (Semantic Engine V2 oracle) -----------------------
   These wrap EXISTING interpreter commands (e.g. %:get-type) and emit a small
   JSON envelope so the JS-side engine gets structured {ok,type} data instead
   of parsing free-form text. No core command is added or modified. *)

let json_escape s =
  let b = Buffer.create (String.length s + 2) in
  String.iter
    (fun c ->
       match c with
       | '"' -> Buffer.add_string b "\\\""
       | '\\' -> Buffer.add_string b "\\\\"
       | '\n' -> Buffer.add_string b "\\n"
       | '\r' -> Buffer.add_string b "\\r"
       | '\t' -> Buffer.add_string b "\\t"
       | c when Char.code c < 0x20 ->
         Buffer.add_string b (Printf.sprintf "\\u%04x" (Char.code c))
       | c -> Buffer.add_char b c)
    s;
  Buffer.contents b

let contains_sub s sub =
  let ls = String.length s and lsub = String.length sub in
  if lsub = 0 then true
  else if lsub > ls then false
  else
    let rec aux i =
      if i > ls - lsub then false
      else if String.sub s i lsub = sub then true
      else aux (i + 1)
    in
    aux 0

let rstrip_type s =
  (* Drop trailing whitespace and ';' the way the JS parser does. *)
  let n = ref (String.length s) in
  while !n > 0 &&
        (match s.[!n - 1] with ' ' | '\t' | '\n' | '\r' | ';' -> true | _ -> false)
  do decr n done;
  String.sub s 0 !n

(* Mirror live-intel's parseTypeResponse: only forward a response that looks
   like a real type/kind, rejecting Beluga's error / "no info" strings that
   arrive on the same channel. Returns the cleaned type, or None. *)
let valid_type_response raw =
  let text = rstrip_type (String.trim raw) in
  if String.length text = 0 then None
  else if text.[0] = '-' then None
  else
    let low = String.lowercase_ascii text in
    let starts p = String.length low >= String.length p && String.sub low 0 (String.length p) = p in
    if starts "no " || starts "error" then None
    else
      let bad =
        [ "ill-formed"; "ill formed"; "unbound"; "unrecognized"
        ; "cannot"; "no type"; "not defined"; "not found" ]
      in
      if List.exists (contains_sub low) bad then None else Some text

let type_at_json session line col =
  let cmd = Printf.sprintf "%%:get-type %d %d" line col in
  let (raw, ran) = run_command_status session cmd in
  let t = if ran then valid_type_response raw else None in
  let ok = match t with Some _ -> true | None -> false in
  let type_field =
    match t with None -> "null" | Some ty -> "\"" ^ json_escape ty ^ "\""
  in
  Printf.sprintf "{\"ok\":%b,\"type\":%s,\"raw\":\"%s\"}" ok type_field (json_escape raw)

(* Decl-level reconstructed type. Looks up a top-level declaration by NAME in the
   global store (populated by the last successful load) and pretty-prints its
   elaborated type with implicit arguments expanded. Returns {ok,type}.
   Search order: comp program (rec) -> comp constructor -> LF type family kind ->
   LF term constructor, so the primary "rec name" case wins on collisions. LF term
   constructors are reached by walking each type family's constructor list (the
   Term store has no global enumeration, but Typ.Entry.constructors lists them). *)
let decl_type_json requested =
  let open Synint in
  let matches (n : Name.t) = Name.string_of_name n = requested in
  let fmt pp x =
    let out = ref "" in
    Printer.with_implicits true (fun () -> out := Format.asprintf "%a" pp x);
    !out
  in
  (* Find an LF term constructor by name across all type families. *)
  let find_lf_term () =
    List.fold_left
      (fun acc (_, tentry) ->
         match acc with
         | Some _ -> acc
         | None ->
           List.fold_left
             (fun acc cid ->
                match acc with
                | Some _ -> acc
                | None ->
                  let e = Store.Cid.Term.get cid in
                  if matches e.Store.Cid.Term.Entry.name then Some e else None)
             None
             !(tentry.Store.Cid.Typ.Entry.constructors))
      None
      (Store.Cid.Typ.current_entries ())
  in
  let found =
    try
      match
        List.find_opt
          (fun (_, e) -> matches e.Store.Cid.Comp.Entry.name)
          (Store.Cid.Comp.current_entries ())
      with
      | Some (_, e) ->
        Some (fmt (P.fmt_ppr_cmp_typ LF.Empty P.l0) e.Store.Cid.Comp.Entry.typ)
      | None -> (
        match
          List.find_opt
            (fun (_, e) -> matches e.Store.Cid.CompConst.Entry.name)
            (Store.Cid.CompConst.current_entries ())
        with
        | Some (_, e) ->
          Some
            (fmt (P.fmt_ppr_cmp_typ LF.Empty P.l0)
               e.Store.Cid.CompConst.Entry.typ)
        | None -> (
          match
            List.find_opt
              (fun (_, e) -> matches e.Store.Cid.Typ.Entry.name)
              (Store.Cid.Typ.current_entries ())
          with
          | Some (_, e) ->
            Some
              (fmt (P.fmt_ppr_lf_kind LF.Null P.l0) e.Store.Cid.Typ.Entry.kind)
          | None -> (
            match find_lf_term () with
            | Some e ->
              Some
                (fmt
                   (P.fmt_ppr_lf_typ LF.Empty LF.Null P.l0)
                   e.Store.Cid.Term.Entry.typ)
            | None -> None)))
    with _ -> None
  in
  match found with
  | Some ty -> Printf.sprintf "{\"ok\":true,\"type\":\"%s\"}" (json_escape ty)
  | None -> "{\"ok\":false,\"type\":null}"

let command_json session input =
  let (raw, ran) = run_command_status session input in
  Printf.sprintf "{\"ok\":%b,\"output\":\"%s\"}" ran (json_escape raw)

(* JS passes implicit use-sites as "name|line|col;..." (1-based line, 0-based col). *)
let parse_position_triples spec =
  if String.length spec = 0 then []
  else
    List.filter_map
      (fun part ->
         let bits = String.split_on_char '|' part in
         match bits with
         | [ name; ls; cs ] -> (
             try Some (name, int_of_string ls, int_of_string cs)
             with _ -> None)
         | _ -> None)
      (String.split_on_char ';' spec)

let implicit_entry_json name line col ty =
  Printf.sprintf "{\"name\":\"%s\",\"line\":%d,\"col\":%d,\"type\":\"%s\"}"
    (json_escape name) line col (json_escape ty)

let elaborate_decl_json session positions_spec =
  let positions = parse_position_triples positions_spec in
  if positions = [] then
    "{\"ok\":false,\"reason\":\"no-positions\",\"fallback\":\"use-ideTypeAtJson\"}"
  else
    let entries = ref [] in
    List.iter
      (fun (name, line, col) ->
         let cmd = Printf.sprintf "%%:get-type %d %d" line col in
         let raw, ran = run_command_status session cmd in
         let t = if ran then valid_type_response raw else None in
         match t with
         | Some ty -> entries := implicit_entry_json name line col ty :: !entries
         | None -> ())
      positions;
    if !entries = [] then
      "{\"ok\":false,\"reason\":\"no-types\",\"implicits\":[]}"
    else
      Printf.sprintf "{\"ok\":true,\"implicits\":[%s],\"metavars\":[],\"diagnostics\":[]}"
        (String.concat "," (List.rev !entries))

let make_check_result ~output ~ok =
  object%js
    val output = Js.string output
    val ok = Js.bool ok
  end

let make_load_result ~output ~ok ~fingerprint =
  object%js
    val output = Js.string output
    val ok = Js.bool ok
    val fingerprint = Js.string fingerprint
  end

let () =
  Printexc.record_backtrace true;
  let session = ref (create ()) in
  let committed_fingerprint = ref "" in
  Js.export "Beluga"
    (object%js
       method create =
          session := create ();
          committed_fingerprint := "";
          Js.string "Beluga session created."

       method checkFromString content =
          let s = Js.to_string content in
          let trial = create () in
          let (out, ok) = load_from_string trial s in
          make_check_result ~output:out ~ok

       method loadFromString content =
          let s = Js.to_string content in
          let trial = create () in
          let (out, ok) = load_from_string trial s in
          if ok then (
            session := trial;
            committed_fingerprint := fingerprint_content s);
          make_load_result ~output:out ~ok ~fingerprint:!committed_fingerprint

       method runCommand input =
          let s = Js.to_string input in
          Js.string (run_command !session s)

       (* Semantic Engine V2 oracle: per-declaration type as a JSON envelope
          {ok, type, raw}, computed against the currently loaded session via
          the existing %:get-type command. Line/col are 1-based, matching
          live-intel's runGetType. *)
       method ideTypeAtJson line col =
          Js.string (type_at_json !session line col)

       (* Decl-level reconstructed type for NAME, with implicits expanded:
          {ok, type}. Reads the global store from the last committed load. *)
       method ideDeclType name =
          Js.string (decl_type_json (Js.to_string name))

       (* Generic JSON wrapper over any interpreter command: {ok, output}.
          `ok` reflects whether the command ran without raising. *)
       method ideCommandJson input =
          let s = Js.to_string input in
          Js.string (command_json !session s)

       method getCommittedFingerprint =
          Js.string !committed_fingerprint

       (* Batch elaboration: runs %:get-type at each JS-supplied use-site in one
          WASM call. Tier B (upstream %:elaborate-decl / Typeinfo walk) can replace
          the internal loop when available in Beluga core. *)
       method ideElaborateDecl _start_line _end_line positions_spec =
          let spec = Js.to_string positions_spec in
          Js.string (elaborate_decl_json !session spec)

       method reset =
          session := create ();
          committed_fingerprint := "";
          Js.string "Session reset."
    end)

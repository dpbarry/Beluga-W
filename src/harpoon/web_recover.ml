(* Web-safe Harpoon proof recovery. See web_recover.mli.

   recover_theorem / run_automation are lifted from HarpoonState (the wizard-free
   recovery path), dropping the driver-only coupling (Io.t, Options, Session, the
   prover state record). Tactics act on a Theorem.t + proof_state directly, so the
   Session grouping the driver uses is unnecessary. This links in the web build. *)

open Support
open Beluga_syntax.Syncom
open Beluga_syntax.Synint

let dprintf, _, _ = Debug.(makeFunctions' (toFlags [14]))
open Debug.Fmt

(* open beluga late, as HarpoonState does, so the earlier Synint refs resolve. *)
open Beluga

type located =
  { theorem : Theorem.t
  ; proof_state : Comp.proof_state
  }

(* Lifted from HarpoonState.recover_theorem. Builds a Theorem.t for a cid from
   the (already-reconstructed) stored proof plus its open subgoals. *)
let recover_theorem ppf hooks (cid, gs) =
  let open Comp in
  let e = Store.Cid.Comp.get cid in
  let tau = e.Store.Cid.Comp.Entry.typ in
  let decl = Store.Cid.Comp.get_total_decl cid in
  let initial_state =
    let s =
      make_proof_state SubgoalPath.start
        ( Total.annotate Beluga_syntax.Location.ghost decl.Comp.order tau
        , Whnf.m_id )
    in
    let prf =
      match e.Store.Cid.Comp.Entry.prog with
      | Option.Some (ThmValue (_, Proof p, _, _)) -> p
      | _ -> Error.raise_violation "recovered theorem not a proof"
    in
    s.solution := Some prf;
    s
  in
  Theorem.configure cid ppf hooks initial_state (List1.to_list gs)

let run_automation auto_state (t : Theorem.t) (g : Comp.proof_state) =
  ignore (Automation.execute auto_state t g)

let recover_theorems ?auto ppf =
  let gs = Holes.get_harpoon_subgoals () in
  let hooks = match auto with Some a -> [run_automation a] | None -> [] in
  dprintf begin fun p ->
    p.fmt "[%s] recovering from %d subgoals" __FUNCTION__ (List.length gs)
    end;
  List1.group_by (fun (_location, theorem_cid, _state) -> theorem_cid) gs
  |> List.map
       (fun (theorem_cid, subgoals) ->
          let subgoals' =
            List1.map (fun (_location, _cid, proof_state) -> proof_state) subgoals
          in
          recover_theorem ppf hooks (theorem_cid, subgoals'))

let all_subgoals theorems =
  List.concat_map
    (fun theorem ->
       List.map
         (fun proof_state -> { theorem; proof_state })
         (Theorem.subgoals theorem))
    theorems

(* Match a recovered located subgoal back to an open_subgoal tuple by cid and
   subgoal path. *)
let locate_for theorems cid ps =
  List.find_opt
    (fun l ->
       Theorem.has_cid_of l.theorem cid
       && Whnf.conv_subgoal_path_builder l.proof_state.Comp.label ps.Comp.label)
    (all_subgoals theorems)

(* Lifted from Prover.elaborate_mvar (the Msplit path). Resolves a meta-variable
   by name in cD and builds the synthesizable expression + type by hand — no
   indexing state, so it links web-side. *)
module S = Substitution

let elaborate_mvar cD loc name =
  let p (d, _) = Name.(LF.name_of_ctyp_decl d = name) in
  match Context.find_with_index_rev' cD p with
  | None -> Lfrecon.(throw loc (UnboundName name))
  | Some LF.(Decl { typ = cT; _ }, k) ->
      let cT = Whnf.cnormMTyp (cT, LF.MShift k) in
      let mF =
        let open LF in
        match cT with
        | ClTyp (mT, cPsi) ->
          let psi_hat = Context.dctxToHat cPsi in
          let obj =
            match mT with
            | MTyp _ -> MObj (MVar (Offset k, S.LF.id) |> head)
            | PTyp _ -> PObj (PVar (k, S.LF.id))
            | STyp _ -> SObj (SVar (k, 0, S.LF.id))
          in
          ClObj (psi_hat, obj)
        | LF.CTyp _ ->
          let cPsi = LF.(CtxVar (CtxOffset k)) in
          CObj cPsi
      in
      let i = Comp.AnnBox (Beluga_syntax.Location.ghost, (loc, mF), cT)
      and tau = Comp.TypBox (loc, cT) in
      (i, tau)
  | _ -> Error.raise_violation "[web_recover] [elaborate_mvar] cD decl has no type"

(* Resolve a COMPUTATION-context variable by name in cG and build its
   synthesizable expression (a de-Bruijn Comp.Var) + type — the cG counterpart of
   elaborate_mvar. This is what lets `split x` work on a boxed comp hypothesis
   like `x : [g |- dual A A']` (the induction scrutinee), which lives in cG, not
   cD. No indexing state, so it links web-side; we read the index/type straight
   off cG the way Prover's elaborate_synthesizing_expression would after indexing
   a bare variable. *)
let elaborate_cvar cG loc name =
  let p (d, _) = Name.(Comp.name_of_ctyp_decl d = name) in
  match Context.find_with_index_rev' cG p with
  | None -> Lfrecon.(throw loc (UnboundName name))
  | Some (Comp.CTypDecl (_, tau, _), k) ->
      (* cG types are stored relative to their position; the k-th entry's type is
         already valid in the full cG/cD at the subgoal, so use it as-is. *)
      let i = Comp.Var (loc, k) in
      (i, tau)
  | Some _ ->
      Error.raise_violation "[web_recover] [elaborate_cvar] cG decl has no type"

let find_at theorems line col =
  let holes = Holes.get_harpoon_subgoals () in
  let exact =
    List.find_opt
      (fun (loc, _cid, _ps) ->
         Location.start_line loc = line && Location.start_column loc = col)
      holes
  in
  match exact with
  | Some (_loc, cid, ps) -> locate_for theorems cid ps
  | None ->
      let on_line = List.filter (fun (loc, _, _) -> Location.start_line loc = line) holes in
      (match on_line with
       | (_loc, cid, ps) :: _ -> locate_for theorems cid ps
       | [] -> (match all_subgoals theorems with l :: _ -> Some l | [] -> None))

(** Web-safe Harpoon proof recovery.

    This module exposes the wizard-free path by which Harpoon configures real
    [Theorem.t] proof states from the open subgoals (`?` holes) that a normal
    signature load already produces. It is the seam that lets BelJar's Proof Lab
    drive the genuine Harpoon tactic engine under js_of_ocaml — no terminal
    [Io.t], no file round-trip, no configuration wizard, and crucially no
    [Session] (which carries the terminal-coupled configuration wizard).

    The logic is lifted from {!module:HarpoonState} (recover_theorem /
    run_automation), with the driver-only coupling removed. Tactics operate on a
    [Theorem.t] + [proof_state] directly, so the [Session] grouping the driver
    uses is unnecessary here. It depends only on [harpoon_core] + [beluga], so it
    links in the web build. *)

(** A located proof position: the theorem being proven and one of its open
    subgoals. *)
type located =
  { theorem : Theorem.t
  ; proof_state : Beluga_syntax.Synint.Comp.proof_state
  }

(** [recover_theorems ?auto ppf] reconstructs a [Theorem.t] for every distinct
    theorem that owns an open subgoal in the currently-loaded global state
    (populated by the preceding signature load). Theorems are grouped by their
    cid; mutual grouping (a driver concern) is not reconstructed.

    When [auto] is given, its automation state drives the standard
    [auto_intros]/[auto_solve_trivial] hook on every (initial and split-created)
    subgoal. Omit [auto] for the interactive Proof Lab so that exactly one user
    tactic equals one undoable history action (automation as a hook records
    extra interleaved actions that make single-step undo of a split surprising).
    The Lab instead runs automation on demand as an explicit `auto` tactic. *)
val recover_theorems :
  ?auto:Automation.State.t -> Format.formatter -> Theorem.t list

(** All open subgoals across the given theorems, paired with their owning
    theorem, in recovery order. *)
val all_subgoals : Theorem.t list -> located list

(** [find_at theorems line col] returns the located subgoal whose goal hole is
    at the given 1-based source position, if any (identity by the open
    subgoal's source location). Falls back to any subgoal on the line, then to
    the first subgoal. *)
val find_at : Theorem.t list -> int -> int -> located option

(** [elaborate_mvar cD loc name] resolves the meta-variable [name] in the
    meta-context [cD] to a synthesizable expression and its type, for splitting
    on a context/meta variable. This is the web-safe path lifted from
    {!module:Prover} (Msplit): it walks [cD] by name directly and builds the
    expression by hand, so it needs no indexing state. Raises if [name] is
    unbound in [cD]. *)
val elaborate_mvar :
     Beluga_syntax.Synint.LF.mctx
  -> Beluga_syntax.Location.t
  -> Beluga_syntax.Syncom.Name.t
  -> Beluga_syntax.Synint.Comp.exp * Beluga_syntax.Synint.Comp.typ

(** [elaborate_cvar cG loc name] resolves the COMPUTATION-context variable [name]
    in [cG] to a synthesizable expression (a de-Bruijn {!Comp.Var}) and its type,
    for splitting on a boxed comp hypothesis (the induction scrutinee, e.g.
    [x : \[g |- dual A A'\]]) — the cG counterpart of {!elaborate_mvar}. Web-safe:
    reads the index/type straight off [cG], no indexing state. Raises if [name] is
    unbound in [cG]. *)
val elaborate_cvar :
     Beluga_syntax.Synint.Comp.gctx
  -> Beluga_syntax.Location.t
  -> Beluga_syntax.Syncom.Name.t
  -> Beluga_syntax.Synint.Comp.exp * Beluga_syntax.Synint.Comp.typ

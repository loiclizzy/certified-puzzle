From Stdlib Require Import List Bool.
Import ListNotations.

Require Import CalendarPuzzle.
Require Import DateEnumeration.
Require Import GeneratedSolutions.

Definition date_eqb (x y : nat * nat) : bool :=
  Nat.eqb (fst x) (fst y) &&
  Nat.eqb (snd x) (snd y).

Fixpoint lookup_solution
  (month day : nat)
  (table : list ((nat * nat) * list placement))
  : option (list placement) :=
  match table with
  | [] => None
  | ((m, d), cert) :: rest =>
      if date_eqb (month, day) (m, d)
      then Some cert
      else lookup_solution month day rest
  end.

Definition certified_dateb (md : nat * nat) : bool :=
  let '(month, day) := md in
  match lookup_solution month day solution_table with
  | Some cert => verify month day cert
  | None => false
  end.

Definition all_valid_dates_certifiedb : bool :=
  forallb certified_dateb valid_dates.

(** The only large computational proof:
    all 366 precomputed certificates are present and accepted by [verify]. *)
Theorem all_valid_dates_certified :
  all_valid_dates_certifiedb = true.
Proof.
  vm_compute.
  reflexivity.
Qed.

(** The final theorem is deliberately small:
    finite-domain membership + one reflected computation + verifier soundness. *)
Theorem every_valid_date_has_solution :
  forall month day,
    valid_date month day ->
    exists cert, valid_solution month day cert.
Proof.
  intros month day Hdate.

  assert (Hchecked : certified_dateb (month, day) = true).
  {
    pose proof all_valid_dates_certified as Hall.
    unfold all_valid_dates_certifiedb in Hall.
    unfold valid_date in Hdate.
    pose proof
      (proj1
         (@forallb_forall
            (nat * nat)%type
            certified_dateb
            valid_dates)
         Hall) as Hall_forall.
    exact (Hall_forall (month, day) Hdate).
  }

  unfold certified_dateb in Hchecked.
  destruct
    (lookup_solution month day solution_table)
    as [cert |] eqn:Hlookup.
  - exists cert.
    now apply verify_sound.
  - discriminate.
Qed.

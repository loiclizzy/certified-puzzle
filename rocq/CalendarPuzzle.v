From Stdlib Require Import List Bool Arith.PeanoNat.
Import ListNotations.

(** Calendar puzzle model and a certified Boolean verifier.

    Coordinates are zero based: [(row, column)].
    Input months are 1..12 and days are 1..31.
*)

Definition cell := (nat * nat)%type.
Definition xy (r c : nat) : cell := (r, c).

Definition cell_eqb (x y : cell) : bool :=
  Nat.eqb (fst x) (fst y) && Nat.eqb (snd x) (snd y).

Lemma cell_eqb_eq :
  forall x y, cell_eqb x y = true <-> x = y.
Proof.
  intros [xr xc] [yr yc].
  unfold cell_eqb; simpl.
  rewrite Bool.andb_true_iff.
  rewrite !Nat.eqb_eq.
  split.
  - intros [Hr Hc]. now subst.
  - intros H. inversion H. auto.
Qed.

Inductive piece_id : Type :=
| A | B | C | D | E | F | G | H.

Definition piece_id_eq_dec :
  forall x y : piece_id, {x = y} + {x <> y}.
Proof. decide equality. Defined.

Definition piece_eqb (x y : piece_id) : bool :=
  if piece_id_eq_dec x y then true else false.

Lemma piece_eqb_eq :
  forall x y, piece_eqb x y = true <-> x = y.
Proof.
  intros x y. unfold piece_eqb.
  destruct (piece_id_eq_dec x y) as [Heq | Hneq].
  - split; auto.
  - split; intro H; [discriminate | contradiction].
Qed.

Definition all_pieces : list piece_id := [A; B; C; D; E; F; G; H].

(** The 43 legal board cells. *)
Definition board_cells : list cell :=
  [xy 0 0; xy 0 1; xy 0 2; xy 0 3; xy 0 4; xy 0 5;
   xy 1 0; xy 1 1; xy 1 2; xy 1 3; xy 1 4; xy 1 5;
   xy 2 0; xy 2 1; xy 2 2; xy 2 3; xy 2 4; xy 2 5; xy 2 6;
   xy 3 0; xy 3 1; xy 3 2; xy 3 3; xy 3 4; xy 3 5; xy 3 6;
   xy 4 0; xy 4 1; xy 4 2; xy 4 3; xy 4 4; xy 4 5; xy 4 6;
   xy 5 0; xy 5 1; xy 5 2; xy 5 3; xy 5 4; xy 5 5; xy 5 6;
   xy 6 0; xy 6 1; xy 6 2].

Definition month_cells : list cell :=
  [xy 0 0; xy 0 1; xy 0 2; xy 0 3; xy 0 4; xy 0 5;
   xy 1 0; xy 1 1; xy 1 2; xy 1 3; xy 1 4; xy 1 5].

Definition day_cells : list cell :=
  [xy 2 0; xy 2 1; xy 2 2; xy 2 3; xy 2 4; xy 2 5; xy 2 6;
   xy 3 0; xy 3 1; xy 3 2; xy 3 3; xy 3 4; xy 3 5; xy 3 6;
   xy 4 0; xy 4 1; xy 4 2; xy 4 3; xy 4 4; xy 4 5; xy 4 6;
   xy 5 0; xy 5 1; xy 5 2; xy 5 3; xy 5 4; xy 5 5; xy 5 6;
   xy 6 0; xy 6 1; xy 6 2].

Definition target_cells (month day : nat) : option (cell * cell) :=
  if (1 <=? month) && (month <=? 12) &&
     (1 <=? day) && (day <=? 31)
  then
    match nth_error month_cells (month - 1),
          nth_error day_cells (day - 1) with
    | Some mc, Some dc => Some (mc, dc)
    | _, _ => None
    end
  else None.

(** A certificate gives, for each physical piece:
    - piece identity;
    - orientation index;
    - anchor row and column.

    Orientation indices refer to the variant tables below.
*)
Inductive placement : Type :=
| Place : piece_id -> nat -> nat -> nat -> placement.

Definition placement_piece (pl : placement) : piece_id :=
  match pl with Place p _ _ _ => p end.

Definition placement_orientation (pl : placement) : nat :=
  match pl with Place _ o _ _ => o end.

Definition placement_row (pl : placement) : nat :=
  match pl with Place _ _ r _ => r end.

Definition placement_col (pl : placement) : nat :=
  match pl with Place _ _ _ c => c end.

Definition shape := list cell.
Definition variants := list shape.

(** The orientation tables are generated from the eight photographed pieces.
    Reflections are included; symmetric duplicates are removed. *)

Definition variants_A : variants :=
  [[xy 0 0; xy 0 1; xy 0 2; xy 1 2; xy 1 3];
   [xy 0 0; xy 0 1; xy 1 1; xy 1 2; xy 1 3];
   [xy 0 0; xy 1 0; xy 1 1; xy 2 1; xy 3 1];
   [xy 0 0; xy 1 0; xy 2 0; xy 2 1; xy 3 1];
   [xy 0 1; xy 0 2; xy 0 3; xy 1 0; xy 1 1];
   [xy 0 1; xy 1 0; xy 1 1; xy 2 0; xy 3 0];
   [xy 0 1; xy 1 1; xy 2 0; xy 2 1; xy 3 0];
   [xy 0 2; xy 0 3; xy 1 0; xy 1 1; xy 1 2]].

Definition variants_B : variants :=
  [[xy 0 0; xy 0 1; xy 0 2; xy 1 0; xy 1 1; xy 1 2];
   [xy 0 0; xy 0 1; xy 1 0; xy 1 1; xy 2 0; xy 2 1]].

Definition variants_C : variants :=
  [[xy 0 0; xy 0 1; xy 0 2; xy 0 3; xy 1 0];
   [xy 0 0; xy 0 1; xy 0 2; xy 0 3; xy 1 3];
   [xy 0 0; xy 0 1; xy 1 0; xy 2 0; xy 3 0];
   [xy 0 0; xy 0 1; xy 1 1; xy 2 1; xy 3 1];
   [xy 0 0; xy 1 0; xy 1 1; xy 1 2; xy 1 3];
   [xy 0 0; xy 1 0; xy 2 0; xy 3 0; xy 3 1];
   [xy 0 1; xy 1 1; xy 2 1; xy 3 0; xy 3 1];
   [xy 0 3; xy 1 0; xy 1 1; xy 1 2; xy 1 3]].

Definition variants_D : variants :=
  [[xy 0 0; xy 0 1; xy 1 1; xy 2 1; xy 2 2];
   [xy 0 0; xy 1 0; xy 1 1; xy 1 2; xy 2 2];
   [xy 0 1; xy 0 2; xy 1 1; xy 2 0; xy 2 1];
   [xy 0 2; xy 1 0; xy 1 1; xy 1 2; xy 2 0]].

Definition variants_E : variants :=
  [[xy 0 0; xy 0 1; xy 0 2; xy 1 0; xy 2 0];
   [xy 0 0; xy 0 1; xy 0 2; xy 1 2; xy 2 2];
   [xy 0 0; xy 1 0; xy 2 0; xy 2 1; xy 2 2];
   [xy 0 2; xy 1 2; xy 2 0; xy 2 1; xy 2 2]].

Definition variants_F : variants :=
  [[xy 0 0; xy 0 1; xy 0 2; xy 0 3; xy 1 1];
   [xy 0 0; xy 0 1; xy 0 2; xy 0 3; xy 1 2];
   [xy 0 0; xy 1 0; xy 1 1; xy 2 0; xy 3 0];
   [xy 0 0; xy 1 0; xy 2 0; xy 2 1; xy 3 0];
   [xy 0 1; xy 1 0; xy 1 1; xy 1 2; xy 1 3];
   [xy 0 1; xy 1 0; xy 1 1; xy 2 1; xy 3 1];
   [xy 0 1; xy 1 1; xy 2 0; xy 2 1; xy 3 1];
   [xy 0 2; xy 1 0; xy 1 1; xy 1 2; xy 1 3]].

Definition variants_G : variants :=
  [[xy 0 0; xy 0 1; xy 0 2; xy 1 0; xy 1 2];
   [xy 0 0; xy 0 1; xy 1 0; xy 2 0; xy 2 1];
   [xy 0 0; xy 0 1; xy 1 1; xy 2 0; xy 2 1];
   [xy 0 0; xy 0 2; xy 1 0; xy 1 1; xy 1 2]].

Definition variants_H : variants :=
  [[xy 0 0; xy 0 1; xy 0 2; xy 1 0; xy 1 1];
   [xy 0 0; xy 0 1; xy 0 2; xy 1 1; xy 1 2];
   [xy 0 0; xy 0 1; xy 1 0; xy 1 1; xy 1 2];
   [xy 0 0; xy 0 1; xy 1 0; xy 1 1; xy 2 0];
   [xy 0 0; xy 0 1; xy 1 0; xy 1 1; xy 2 1];
   [xy 0 0; xy 1 0; xy 1 1; xy 2 0; xy 2 1];
   [xy 0 1; xy 0 2; xy 1 0; xy 1 1; xy 1 2];
   [xy 0 1; xy 1 0; xy 1 1; xy 2 0; xy 2 1]].

Definition variants_of (p : piece_id) : variants :=
  match p with
  | A => variants_A | B => variants_B
  | C => variants_C | D => variants_D
  | E => variants_E | F => variants_F
  | G => variants_G | H => variants_H
  end.

Definition translate (r c : nat) (s : shape) : list cell :=
  map (fun q => xy (r + fst q) (c + snd q)) s.

Definition placement_cells (pl : placement) : option (list cell) :=
  match nth_error (variants_of (placement_piece pl))
                  (placement_orientation pl) with
  | Some s => Some (translate (placement_row pl) (placement_col pl) s)
  | None => None
  end.

Fixpoint collect_cells (cert : list placement) : option (list cell) :=
  match cert with
  | [] => Some []
  | pl :: rest =>
      match placement_cells pl, collect_cells rest with
      | Some xs, Some ys => Some (xs ++ ys)
      | _, _ => None
      end
  end.

Fixpoint nodupb {X : Type} (eqb : X -> X -> bool) (xs : list X) : bool :=
  match xs with
  | [] => true
  | x :: rest => negb (existsb (eqb x) rest) && nodupb eqb rest
  end.

Section BooleanEqualityFacts.
  Context {X : Type}.
  Variable eqb : X -> X -> bool.
  Hypothesis eqb_eq : forall x y, eqb x y = true <-> x = y.

  Lemma existsb_eqb_true_iff :
    forall x xs, existsb (eqb x) xs = true <-> In x xs.
  Proof.
    intros x xs. rewrite existsb_exists.
    split.
    - intros [y [Hy Hxy]].
      apply eqb_eq in Hxy. now subst.
    - intros Hx. exists x. split; [exact Hx |].
      apply eqb_eq. reflexivity.
  Qed.

  Lemma existsb_eqb_false_notin :
    forall x xs, existsb (eqb x) xs = false -> ~ In x xs.
  Proof.
    intros x xs Hfalse Hin.
    pose proof (proj2 (existsb_eqb_true_iff x xs) Hin) as Htrue.
    rewrite Hfalse in Htrue. discriminate.
  Qed.

  Lemma nodupb_sound :
    forall xs, nodupb eqb xs = true -> NoDup xs.
  Proof.
    induction xs as [|x rest IH]; simpl; intro H.
    - constructor.
    - apply Bool.andb_true_iff in H as [Hhead Htail].
      constructor.
      + apply Bool.negb_true_iff in Hhead.
        now apply (existsb_eqb_false_notin x rest).
      + now apply IH.
  Qed.
End BooleanEqualityFacts.

Definition in_boardb (c : cell) : bool :=
  existsb (cell_eqb c) board_cells.

Definition pieces_presentb (ids : list piece_id) : bool :=
  forallb (fun p => existsb (piece_eqb p) ids) all_pieces.

Definition verify (month day : nat) (cert : list placement) : bool :=
  match target_cells month day, collect_cells cert with
  | Some (mc, dc), Some cells =>
      Nat.eqb (length cert) 8 &&
      nodupb piece_eqb (map placement_piece cert) &&
      pieces_presentb (map placement_piece cert) &&
      Nat.eqb (length cells) 41 &&
      nodupb cell_eqb cells &&
      forallb in_boardb cells &&
      negb (existsb (cell_eqb mc) cells) &&
      negb (existsb (cell_eqb dc) cells)
  | _, _ => false
  end.

(** A mathematical statement of what acceptance guarantees. *)
Definition valid_solution
  (month day : nat) (cert : list placement) : Prop :=
  exists mc dc cells,
    target_cells month day = Some (mc, dc) /\
    collect_cells cert = Some cells /\
    length cert = 8 /\
    NoDup (map placement_piece cert) /\
    (forall p, In p all_pieces ->
       In p (map placement_piece cert)) /\
    length cells = 41 /\
    NoDup cells /\
    Forall (fun c => In c board_cells) cells /\
    ~ In mc cells /\
    ~ In dc cells.

Lemma in_boardb_sound :
  forall c, in_boardb c = true -> In c board_cells.
Proof.
  intros c H.
  unfold in_boardb in H.
  now apply (proj1
    (@existsb_eqb_true_iff cell cell_eqb cell_eqb_eq c board_cells)).
Qed.

Lemma forallb_in_board_sound :
  forall cells,
    forallb in_boardb cells = true ->
    Forall (fun c => In c board_cells) cells.
Proof.
  induction cells as [| c cs IH]; intros H; simpl in H.
  - constructor.
  - apply Bool.andb_true_iff in H as [Hc Hcs].
    constructor.
    + now apply in_boardb_sound.
    + now apply IH.
Qed.

Theorem verify_sound :
  forall month day cert,
    verify month day cert = true ->
    valid_solution month day cert.
Proof.
  intros month day cert Hverify.
  unfold verify in Hverify.
  destruct (target_cells month day) as [[mc dc] |] eqn:Htarget;
    try discriminate.
  destruct (collect_cells cert) as [cells |] eqn:Hcollect;
    try discriminate.

  apply Bool.andb_true_iff in Hverify as [Hverify Hday].
  apply Bool.andb_true_iff in Hverify as [Hverify Hmonth].
  apply Bool.andb_true_iff in Hverify as [Hverify Hboard].
  apply Bool.andb_true_iff in Hverify as [Hverify Hnodup_cells].
  apply Bool.andb_true_iff in Hverify as [Hverify Hlen_cells].
  apply Bool.andb_true_iff in Hverify as [Hverify Hpresent].
  apply Bool.andb_true_iff in Hverify as [Hlen_cert Hnodup_pieces].

  exists mc, dc, cells.
  repeat split.
  - exact Htarget.
  - exact Hcollect.
  - now apply Nat.eqb_eq in Hlen_cert.
  - now apply
      (@nodupb_sound piece_id piece_eqb piece_eqb_eq
         (map placement_piece cert)).
  - intros p Hp.
    unfold pieces_presentb in Hpresent.
    pose proof
      (proj1 (@forallb_forall
                piece_id
                (fun q =>
                   existsb (piece_eqb q)
                     (map placement_piece cert))
                all_pieces)
             Hpresent) as Hpresent_all.
    specialize (Hpresent_all p Hp).
    now apply (proj1
      (@existsb_eqb_true_iff piece_id piece_eqb piece_eqb_eq
         p (map placement_piece cert))) in Hpresent_all.
  - now apply Nat.eqb_eq in Hlen_cells.
  - now apply
      (@nodupb_sound cell cell_eqb cell_eqb_eq cells).
  - now apply forallb_in_board_sound.
  - apply Bool.negb_true_iff in Hmonth.
    now apply
      (@existsb_eqb_false_notin cell cell_eqb cell_eqb_eq mc cells).
  - apply Bool.negb_true_iff in Hday.
    now apply
      (@existsb_eqb_false_notin cell cell_eqb cell_eqb_eq dc cells).
Qed.

(** A concrete certificate matching the Python solver's MAR 08 example. *)
Definition example_MAR_08 : list placement :=
  [Place A 1 0 0;
   Place B 0 5 0;
   Place C 6 2 5;
   Place D 1 1 0;
   Place E 1 2 3;
   Place F 4 3 0;
   Place G 2 3 3;
   Place H 1 0 3].

Example example_MAR_08_is_accepted :
  verify 3 8 example_MAR_08 = true.
Proof. vm_compute. reflexivity. Qed.

Example example_MAR_08_is_valid :
  valid_solution 3 8 example_MAR_08.
Proof.
  apply verify_sound.
  vm_compute. reflexivity.
Qed.

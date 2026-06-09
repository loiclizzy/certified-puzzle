
(** val negb : bool -> bool **)

let negb = function
| true -> false
| false -> true

(** val fst : ('a1 * 'a2) -> 'a1 **)

let fst = function
| (x, _) -> x

(** val snd : ('a1 * 'a2) -> 'a2 **)

let snd = function
| (_, y) -> y

(** val length : 'a1 list -> int **)

let rec length = function
| [] -> 0
| _ :: l' -> Stdlib.Int.succ (length l')

(** val app : 'a1 list -> 'a1 list -> 'a1 list **)

let rec app l m =
  match l with
  | [] -> m
  | a :: l1 -> a :: (app l1 m)

(** val add : int -> int -> int **)

let rec add = (+)

(** val sub : int -> int -> int **)

let rec sub = fun n m -> Stdlib.max 0 (n-m)

(** val eqb : int -> int -> bool **)

let rec eqb n m =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ ->
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> true)
      (fun _ -> false)
      m)
    (fun n' ->
    (fun fO fS n -> if n=0 then fO () else fS (n-1))
      (fun _ -> false)
      (fun m' -> eqb n' m')
      m)
    n

module Nat =
 struct
 end

(** val map : ('a1 -> 'a2) -> 'a1 list -> 'a2 list **)

let rec map f = function
| [] -> []
| a :: l0 -> (f a) :: (map f l0)

(** val nth_error : 'a1 list -> int -> 'a1 option **)

let rec nth_error l n =
  (fun fO fS n -> if n=0 then fO () else fS (n-1))
    (fun _ -> match l with
              | [] -> None
              | x :: _ -> Some x)
    (fun n0 -> match l with
               | [] -> None
               | _ :: l' -> nth_error l' n0)
    n

(** val existsb : ('a1 -> bool) -> 'a1 list -> bool **)

let rec existsb f = function
| [] -> false
| a :: l0 -> (||) (f a) (existsb f l0)

(** val forallb : ('a1 -> bool) -> 'a1 list -> bool **)

let rec forallb f = function
| [] -> true
| a :: l0 -> (&&) (f a) (forallb f l0)

type cell = int * int

(** val xy : int -> int -> cell **)

let xy r c =
  (r, c)

(** val cell_eqb : cell -> cell -> bool **)

let cell_eqb x y =
  (&&) ((=) (fst x) (fst y)) ((=) (snd x) (snd y))

type piece_id =
| A
| B
| C
| D
| E
| F
| G
| H

(** val piece_id_eq_dec : piece_id -> piece_id -> bool **)

let piece_id_eq_dec x y =
  match x with
  | A -> (match y with
          | A -> true
          | _ -> false)
  | B -> (match y with
          | B -> true
          | _ -> false)
  | C -> (match y with
          | C -> true
          | _ -> false)
  | D -> (match y with
          | D -> true
          | _ -> false)
  | E -> (match y with
          | E -> true
          | _ -> false)
  | F -> (match y with
          | F -> true
          | _ -> false)
  | G -> (match y with
          | G -> true
          | _ -> false)
  | H -> (match y with
          | H -> true
          | _ -> false)

(** val piece_eqb : piece_id -> piece_id -> bool **)

let piece_eqb x y =
  if piece_id_eq_dec x y then true else false

(** val all_pieces : piece_id list **)

let all_pieces =
  A :: (B :: (C :: (D :: (E :: (F :: (G :: (H :: [])))))))

(** val board_cells : cell list **)

let board_cells =
  (xy 0 0) :: ((xy 0 (Stdlib.Int.succ 0)) :: ((xy 0 (Stdlib.Int.succ
                                                (Stdlib.Int.succ 0))) :: (
    (xy 0 (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: (
    (xy 0 (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
      0))))) :: ((xy 0 (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                   (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((xy
                                                                   (Stdlib.Int.succ
                                                                   0) 0) :: (
    (xy (Stdlib.Int.succ 0) (Stdlib.Int.succ 0)) :: ((xy (Stdlib.Int.succ 0)
                                                       (Stdlib.Int.succ
                                                       (Stdlib.Int.succ 0))) :: (
    (xy (Stdlib.Int.succ 0) (Stdlib.Int.succ (Stdlib.Int.succ
      (Stdlib.Int.succ 0)))) :: ((xy (Stdlib.Int.succ 0) (Stdlib.Int.succ
                                   (Stdlib.Int.succ (Stdlib.Int.succ
                                   (Stdlib.Int.succ 0))))) :: ((xy
                                                                 (Stdlib.Int.succ
                                                                 0)
                                                                 (Stdlib.Int.succ
                                                                 (Stdlib.Int.succ
                                                                 (Stdlib.Int.succ
                                                                 (Stdlib.Int.succ
                                                                 (Stdlib.Int.succ
                                                                 0)))))) :: (
    (xy (Stdlib.Int.succ (Stdlib.Int.succ 0)) 0) :: ((xy (Stdlib.Int.succ
                                                       (Stdlib.Int.succ 0))
                                                       (Stdlib.Int.succ 0)) :: (
    (xy (Stdlib.Int.succ (Stdlib.Int.succ 0)) (Stdlib.Int.succ
      (Stdlib.Int.succ 0))) :: ((xy (Stdlib.Int.succ (Stdlib.Int.succ 0))
                                  (Stdlib.Int.succ (Stdlib.Int.succ
                                  (Stdlib.Int.succ 0)))) :: ((xy
                                                               (Stdlib.Int.succ
                                                               (Stdlib.Int.succ
                                                               0))
                                                               (Stdlib.Int.succ
                                                               (Stdlib.Int.succ
                                                               (Stdlib.Int.succ
                                                               (Stdlib.Int.succ
                                                               0))))) :: (
    (xy (Stdlib.Int.succ (Stdlib.Int.succ 0)) (Stdlib.Int.succ
      (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
      0)))))) :: ((xy (Stdlib.Int.succ (Stdlib.Int.succ 0)) (Stdlib.Int.succ
                    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((xy
                                                                    (Stdlib.Int.succ
                                                                    (Stdlib.Int.succ
                                                                    (Stdlib.Int.succ
                                                                    0))) 0) :: (
    (xy (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))
      (Stdlib.Int.succ 0)) :: ((xy (Stdlib.Int.succ (Stdlib.Int.succ
                                 (Stdlib.Int.succ 0))) (Stdlib.Int.succ
                                 (Stdlib.Int.succ 0))) :: ((xy
                                                             (Stdlib.Int.succ
                                                             (Stdlib.Int.succ
                                                             (Stdlib.Int.succ
                                                             0)))
                                                             (Stdlib.Int.succ
                                                             (Stdlib.Int.succ
                                                             (Stdlib.Int.succ
                                                             0)))) :: (
    (xy (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))
      (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
      0))))) :: ((xy (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))
                   (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                   (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((xy
                                                                   (Stdlib.Int.succ
                                                                   (Stdlib.Int.succ
                                                                   (Stdlib.Int.succ
                                                                   0)))
                                                                   (Stdlib.Int.succ
                                                                   (Stdlib.Int.succ
                                                                   (Stdlib.Int.succ
                                                                   (Stdlib.Int.succ
                                                                   (Stdlib.Int.succ
                                                                   (Stdlib.Int.succ
                                                                   0))))))) :: (
    (xy (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
      0)))) 0) :: ((xy (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                     (Stdlib.Int.succ 0)))) (Stdlib.Int.succ 0)) :: (
    (xy (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
      0)))) (Stdlib.Int.succ (Stdlib.Int.succ 0))) :: ((xy (Stdlib.Int.succ
                                                         (Stdlib.Int.succ
                                                         (Stdlib.Int.succ
                                                         (Stdlib.Int.succ
                                                         0))))
                                                         (Stdlib.Int.succ
                                                         (Stdlib.Int.succ
                                                         (Stdlib.Int.succ
                                                         0)))) :: ((xy
                                                                    (Stdlib.Int.succ
                                                                    (Stdlib.Int.succ
                                                                    (Stdlib.Int.succ
                                                                    (Stdlib.Int.succ
                                                                    0))))
                                                                    (Stdlib.Int.succ
                                                                    (Stdlib.Int.succ
                                                                    (Stdlib.Int.succ
                                                                    (Stdlib.Int.succ
                                                                    0))))) :: (
    (xy (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
      0)))) (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
      (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((xy (Stdlib.Int.succ
                                                      (Stdlib.Int.succ
                                                      (Stdlib.Int.succ
                                                      (Stdlib.Int.succ 0))))
                                                      (Stdlib.Int.succ
                                                      (Stdlib.Int.succ
                                                      (Stdlib.Int.succ
                                                      (Stdlib.Int.succ
                                                      (Stdlib.Int.succ
                                                      (Stdlib.Int.succ
                                                      0))))))) :: ((xy
                                                                    (Stdlib.Int.succ
                                                                    (Stdlib.Int.succ
                                                                    (Stdlib.Int.succ
                                                                    (Stdlib.Int.succ
                                                                    (Stdlib.Int.succ
                                                                    0))))) 0) :: (
    (xy (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
      (Stdlib.Int.succ 0))))) (Stdlib.Int.succ 0)) :: ((xy (Stdlib.Int.succ
                                                         (Stdlib.Int.succ
                                                         (Stdlib.Int.succ
                                                         (Stdlib.Int.succ
                                                         (Stdlib.Int.succ
                                                         0)))))
                                                         (Stdlib.Int.succ
                                                         (Stdlib.Int.succ 0))) :: (
    (xy (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
      (Stdlib.Int.succ 0))))) (Stdlib.Int.succ (Stdlib.Int.succ
      (Stdlib.Int.succ 0)))) :: ((xy (Stdlib.Int.succ (Stdlib.Int.succ
                                   (Stdlib.Int.succ (Stdlib.Int.succ
                                   (Stdlib.Int.succ 0))))) (Stdlib.Int.succ
                                   (Stdlib.Int.succ (Stdlib.Int.succ
                                   (Stdlib.Int.succ 0))))) :: ((xy
                                                                 (Stdlib.Int.succ
                                                                 (Stdlib.Int.succ
                                                                 (Stdlib.Int.succ
                                                                 (Stdlib.Int.succ
                                                                 (Stdlib.Int.succ
                                                                 0)))))
                                                                 (Stdlib.Int.succ
                                                                 (Stdlib.Int.succ
                                                                 (Stdlib.Int.succ
                                                                 (Stdlib.Int.succ
                                                                 (Stdlib.Int.succ
                                                                 0)))))) :: (
    (xy (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
      (Stdlib.Int.succ 0))))) (Stdlib.Int.succ (Stdlib.Int.succ
      (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
      0))))))) :: ((xy (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                     (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                     0)))))) 0) :: ((xy (Stdlib.Int.succ (Stdlib.Int.succ
                                      (Stdlib.Int.succ (Stdlib.Int.succ
                                      (Stdlib.Int.succ (Stdlib.Int.succ
                                      0)))))) (Stdlib.Int.succ 0)) :: (
    (xy (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
      (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) (Stdlib.Int.succ
      (Stdlib.Int.succ 0))) :: []))))))))))))))))))))))))))))))))))))))))))

(** val month_cells : cell list **)

let month_cells =
  (xy 0 0) :: ((xy 0 (Stdlib.Int.succ 0)) :: ((xy 0 (Stdlib.Int.succ
                                                (Stdlib.Int.succ 0))) :: (
    (xy 0 (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: (
    (xy 0 (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
      0))))) :: ((xy 0 (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                   (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((xy
                                                                   (Stdlib.Int.succ
                                                                   0) 0) :: (
    (xy (Stdlib.Int.succ 0) (Stdlib.Int.succ 0)) :: ((xy (Stdlib.Int.succ 0)
                                                       (Stdlib.Int.succ
                                                       (Stdlib.Int.succ 0))) :: (
    (xy (Stdlib.Int.succ 0) (Stdlib.Int.succ (Stdlib.Int.succ
      (Stdlib.Int.succ 0)))) :: ((xy (Stdlib.Int.succ 0) (Stdlib.Int.succ
                                   (Stdlib.Int.succ (Stdlib.Int.succ
                                   (Stdlib.Int.succ 0))))) :: ((xy
                                                                 (Stdlib.Int.succ
                                                                 0)
                                                                 (Stdlib.Int.succ
                                                                 (Stdlib.Int.succ
                                                                 (Stdlib.Int.succ
                                                                 (Stdlib.Int.succ
                                                                 (Stdlib.Int.succ
                                                                 0)))))) :: [])))))))))))

(** val day_cells : cell list **)

let day_cells =
  (xy (Stdlib.Int.succ (Stdlib.Int.succ 0)) 0) :: ((xy (Stdlib.Int.succ
                                                     (Stdlib.Int.succ 0))
                                                     (Stdlib.Int.succ 0)) :: (
    (xy (Stdlib.Int.succ (Stdlib.Int.succ 0)) (Stdlib.Int.succ
      (Stdlib.Int.succ 0))) :: ((xy (Stdlib.Int.succ (Stdlib.Int.succ 0))
                                  (Stdlib.Int.succ (Stdlib.Int.succ
                                  (Stdlib.Int.succ 0)))) :: ((xy
                                                               (Stdlib.Int.succ
                                                               (Stdlib.Int.succ
                                                               0))
                                                               (Stdlib.Int.succ
                                                               (Stdlib.Int.succ
                                                               (Stdlib.Int.succ
                                                               (Stdlib.Int.succ
                                                               0))))) :: (
    (xy (Stdlib.Int.succ (Stdlib.Int.succ 0)) (Stdlib.Int.succ
      (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
      0)))))) :: ((xy (Stdlib.Int.succ (Stdlib.Int.succ 0)) (Stdlib.Int.succ
                    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((xy
                                                                    (Stdlib.Int.succ
                                                                    (Stdlib.Int.succ
                                                                    (Stdlib.Int.succ
                                                                    0))) 0) :: (
    (xy (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))
      (Stdlib.Int.succ 0)) :: ((xy (Stdlib.Int.succ (Stdlib.Int.succ
                                 (Stdlib.Int.succ 0))) (Stdlib.Int.succ
                                 (Stdlib.Int.succ 0))) :: ((xy
                                                             (Stdlib.Int.succ
                                                             (Stdlib.Int.succ
                                                             (Stdlib.Int.succ
                                                             0)))
                                                             (Stdlib.Int.succ
                                                             (Stdlib.Int.succ
                                                             (Stdlib.Int.succ
                                                             0)))) :: (
    (xy (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))
      (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
      0))))) :: ((xy (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))
                   (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                   (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((xy
                                                                   (Stdlib.Int.succ
                                                                   (Stdlib.Int.succ
                                                                   (Stdlib.Int.succ
                                                                   0)))
                                                                   (Stdlib.Int.succ
                                                                   (Stdlib.Int.succ
                                                                   (Stdlib.Int.succ
                                                                   (Stdlib.Int.succ
                                                                   (Stdlib.Int.succ
                                                                   (Stdlib.Int.succ
                                                                   0))))))) :: (
    (xy (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
      0)))) 0) :: ((xy (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                     (Stdlib.Int.succ 0)))) (Stdlib.Int.succ 0)) :: (
    (xy (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
      0)))) (Stdlib.Int.succ (Stdlib.Int.succ 0))) :: ((xy (Stdlib.Int.succ
                                                         (Stdlib.Int.succ
                                                         (Stdlib.Int.succ
                                                         (Stdlib.Int.succ
                                                         0))))
                                                         (Stdlib.Int.succ
                                                         (Stdlib.Int.succ
                                                         (Stdlib.Int.succ
                                                         0)))) :: ((xy
                                                                    (Stdlib.Int.succ
                                                                    (Stdlib.Int.succ
                                                                    (Stdlib.Int.succ
                                                                    (Stdlib.Int.succ
                                                                    0))))
                                                                    (Stdlib.Int.succ
                                                                    (Stdlib.Int.succ
                                                                    (Stdlib.Int.succ
                                                                    (Stdlib.Int.succ
                                                                    0))))) :: (
    (xy (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
      0)))) (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
      (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((xy (Stdlib.Int.succ
                                                      (Stdlib.Int.succ
                                                      (Stdlib.Int.succ
                                                      (Stdlib.Int.succ 0))))
                                                      (Stdlib.Int.succ
                                                      (Stdlib.Int.succ
                                                      (Stdlib.Int.succ
                                                      (Stdlib.Int.succ
                                                      (Stdlib.Int.succ
                                                      (Stdlib.Int.succ
                                                      0))))))) :: ((xy
                                                                    (Stdlib.Int.succ
                                                                    (Stdlib.Int.succ
                                                                    (Stdlib.Int.succ
                                                                    (Stdlib.Int.succ
                                                                    (Stdlib.Int.succ
                                                                    0))))) 0) :: (
    (xy (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
      (Stdlib.Int.succ 0))))) (Stdlib.Int.succ 0)) :: ((xy (Stdlib.Int.succ
                                                         (Stdlib.Int.succ
                                                         (Stdlib.Int.succ
                                                         (Stdlib.Int.succ
                                                         (Stdlib.Int.succ
                                                         0)))))
                                                         (Stdlib.Int.succ
                                                         (Stdlib.Int.succ 0))) :: (
    (xy (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
      (Stdlib.Int.succ 0))))) (Stdlib.Int.succ (Stdlib.Int.succ
      (Stdlib.Int.succ 0)))) :: ((xy (Stdlib.Int.succ (Stdlib.Int.succ
                                   (Stdlib.Int.succ (Stdlib.Int.succ
                                   (Stdlib.Int.succ 0))))) (Stdlib.Int.succ
                                   (Stdlib.Int.succ (Stdlib.Int.succ
                                   (Stdlib.Int.succ 0))))) :: ((xy
                                                                 (Stdlib.Int.succ
                                                                 (Stdlib.Int.succ
                                                                 (Stdlib.Int.succ
                                                                 (Stdlib.Int.succ
                                                                 (Stdlib.Int.succ
                                                                 0)))))
                                                                 (Stdlib.Int.succ
                                                                 (Stdlib.Int.succ
                                                                 (Stdlib.Int.succ
                                                                 (Stdlib.Int.succ
                                                                 (Stdlib.Int.succ
                                                                 0)))))) :: (
    (xy (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
      (Stdlib.Int.succ 0))))) (Stdlib.Int.succ (Stdlib.Int.succ
      (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
      0))))))) :: ((xy (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                     (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                     0)))))) 0) :: ((xy (Stdlib.Int.succ (Stdlib.Int.succ
                                      (Stdlib.Int.succ (Stdlib.Int.succ
                                      (Stdlib.Int.succ (Stdlib.Int.succ
                                      0)))))) (Stdlib.Int.succ 0)) :: (
    (xy (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
      (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) (Stdlib.Int.succ
      (Stdlib.Int.succ 0))) :: []))))))))))))))))))))))))))))))

(** val target_cells : int -> int -> (cell * cell) option **)

let target_cells month day =
  if (&&)
       ((&&)
         ((&&) ((<=) (Stdlib.Int.succ 0) month)
           ((<=) month (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
             (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
             (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
             (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))))))))))
         ((<=) (Stdlib.Int.succ 0) day))
       ((<=) day (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
         (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
         (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
         (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
         (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
         (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
         (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
         (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
         0))))))))))))))))))))))))))))))))
  then (match nth_error month_cells (sub month (Stdlib.Int.succ 0)) with
        | Some mc ->
          (match nth_error day_cells (sub day (Stdlib.Int.succ 0)) with
           | Some dc -> Some (mc, dc)
           | None -> None)
        | None -> None)
  else None

type placement =
| Place of piece_id * int * int * int

(** val placement_piece : placement -> piece_id **)

let placement_piece = function
| Place (p, _, _, _) -> p

(** val placement_orientation : placement -> int **)

let placement_orientation = function
| Place (_, o, _, _) -> o

(** val placement_row : placement -> int **)

let placement_row = function
| Place (_, _, r, _) -> r

(** val placement_col : placement -> int **)

let placement_col = function
| Place (_, _, _, c) -> c

type shape = cell list

type variants = shape list

(** val variants_A : variants **)

let variants_A =
  ((xy 0 0) :: ((xy 0 (Stdlib.Int.succ 0)) :: ((xy 0 (Stdlib.Int.succ
                                                 (Stdlib.Int.succ 0))) :: (
    (xy (Stdlib.Int.succ 0) (Stdlib.Int.succ (Stdlib.Int.succ 0))) :: (
    (xy (Stdlib.Int.succ 0) (Stdlib.Int.succ (Stdlib.Int.succ
      (Stdlib.Int.succ 0)))) :: []))))) :: (((xy 0 0) :: ((xy 0
                                                            (Stdlib.Int.succ
                                                            0)) :: ((xy
                                                                    (Stdlib.Int.succ
                                                                    0)
                                                                    (Stdlib.Int.succ
                                                                    0)) :: (
    (xy (Stdlib.Int.succ 0) (Stdlib.Int.succ (Stdlib.Int.succ 0))) :: (
    (xy (Stdlib.Int.succ 0) (Stdlib.Int.succ (Stdlib.Int.succ
      (Stdlib.Int.succ 0)))) :: []))))) :: (((xy 0 0) :: ((xy
                                                            (Stdlib.Int.succ
                                                            0) 0) :: (
    (xy (Stdlib.Int.succ 0) (Stdlib.Int.succ 0)) :: ((xy (Stdlib.Int.succ
                                                       (Stdlib.Int.succ 0))
                                                       (Stdlib.Int.succ 0)) :: (
    (xy (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))
      (Stdlib.Int.succ 0)) :: []))))) :: (((xy 0 0) :: ((xy (Stdlib.Int.succ
                                                          0) 0) :: ((xy
                                                                    (Stdlib.Int.succ
                                                                    (Stdlib.Int.succ
                                                                    0)) 0) :: (
    (xy (Stdlib.Int.succ (Stdlib.Int.succ 0)) (Stdlib.Int.succ 0)) :: (
    (xy (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))
      (Stdlib.Int.succ 0)) :: []))))) :: (((xy 0 (Stdlib.Int.succ 0)) :: (
    (xy 0 (Stdlib.Int.succ (Stdlib.Int.succ 0))) :: ((xy 0 (Stdlib.Int.succ
                                                       (Stdlib.Int.succ
                                                       (Stdlib.Int.succ 0)))) :: (
    (xy (Stdlib.Int.succ 0) 0) :: ((xy (Stdlib.Int.succ 0) (Stdlib.Int.succ
                                     0)) :: []))))) :: (((xy 0
                                                           (Stdlib.Int.succ
                                                           0)) :: ((xy
                                                                    (Stdlib.Int.succ
                                                                    0) 0) :: (
    (xy (Stdlib.Int.succ 0) (Stdlib.Int.succ 0)) :: ((xy (Stdlib.Int.succ
                                                       (Stdlib.Int.succ 0)) 0) :: (
    (xy (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))) 0) :: []))))) :: ((
    (xy 0 (Stdlib.Int.succ 0)) :: ((xy (Stdlib.Int.succ 0) (Stdlib.Int.succ
                                     0)) :: ((xy (Stdlib.Int.succ
                                               (Stdlib.Int.succ 0)) 0) :: (
    (xy (Stdlib.Int.succ (Stdlib.Int.succ 0)) (Stdlib.Int.succ 0)) :: (
    (xy (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))) 0) :: []))))) :: ((
    (xy 0 (Stdlib.Int.succ (Stdlib.Int.succ 0))) :: ((xy 0 (Stdlib.Int.succ
                                                       (Stdlib.Int.succ
                                                       (Stdlib.Int.succ 0)))) :: (
    (xy (Stdlib.Int.succ 0) 0) :: ((xy (Stdlib.Int.succ 0) (Stdlib.Int.succ
                                     0)) :: ((xy (Stdlib.Int.succ 0)
                                               (Stdlib.Int.succ
                                               (Stdlib.Int.succ 0))) :: []))))) :: [])))))))

(** val variants_B : variants **)

let variants_B =
  ((xy 0 0) :: ((xy 0 (Stdlib.Int.succ 0)) :: ((xy 0 (Stdlib.Int.succ
                                                 (Stdlib.Int.succ 0))) :: (
    (xy (Stdlib.Int.succ 0) 0) :: ((xy (Stdlib.Int.succ 0) (Stdlib.Int.succ
                                     0)) :: ((xy (Stdlib.Int.succ 0)
                                               (Stdlib.Int.succ
                                               (Stdlib.Int.succ 0))) :: [])))))) :: ((
    (xy 0 0) :: ((xy 0 (Stdlib.Int.succ 0)) :: ((xy (Stdlib.Int.succ 0) 0) :: (
    (xy (Stdlib.Int.succ 0) (Stdlib.Int.succ 0)) :: ((xy (Stdlib.Int.succ
                                                       (Stdlib.Int.succ 0)) 0) :: (
    (xy (Stdlib.Int.succ (Stdlib.Int.succ 0)) (Stdlib.Int.succ 0)) :: [])))))) :: [])

(** val variants_C : variants **)

let variants_C =
  ((xy 0 0) :: ((xy 0 (Stdlib.Int.succ 0)) :: ((xy 0 (Stdlib.Int.succ
                                                 (Stdlib.Int.succ 0))) :: (
    (xy 0 (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: (
    (xy (Stdlib.Int.succ 0) 0) :: []))))) :: (((xy 0 0) :: ((xy 0
                                                              (Stdlib.Int.succ
                                                              0)) :: (
    (xy 0 (Stdlib.Int.succ (Stdlib.Int.succ 0))) :: ((xy 0 (Stdlib.Int.succ
                                                       (Stdlib.Int.succ
                                                       (Stdlib.Int.succ 0)))) :: (
    (xy (Stdlib.Int.succ 0) (Stdlib.Int.succ (Stdlib.Int.succ
      (Stdlib.Int.succ 0)))) :: []))))) :: (((xy 0 0) :: ((xy 0
                                                            (Stdlib.Int.succ
                                                            0)) :: ((xy
                                                                    (Stdlib.Int.succ
                                                                    0) 0) :: (
    (xy (Stdlib.Int.succ (Stdlib.Int.succ 0)) 0) :: ((xy (Stdlib.Int.succ
                                                       (Stdlib.Int.succ
                                                       (Stdlib.Int.succ 0)))
                                                       0) :: []))))) :: ((
    (xy 0 0) :: ((xy 0 (Stdlib.Int.succ 0)) :: ((xy (Stdlib.Int.succ 0)
                                                  (Stdlib.Int.succ 0)) :: (
    (xy (Stdlib.Int.succ (Stdlib.Int.succ 0)) (Stdlib.Int.succ 0)) :: (
    (xy (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))
      (Stdlib.Int.succ 0)) :: []))))) :: (((xy 0 0) :: ((xy (Stdlib.Int.succ
                                                          0) 0) :: ((xy
                                                                    (Stdlib.Int.succ
                                                                    0)
                                                                    (Stdlib.Int.succ
                                                                    0)) :: (
    (xy (Stdlib.Int.succ 0) (Stdlib.Int.succ (Stdlib.Int.succ 0))) :: (
    (xy (Stdlib.Int.succ 0) (Stdlib.Int.succ (Stdlib.Int.succ
      (Stdlib.Int.succ 0)))) :: []))))) :: (((xy 0 0) :: ((xy
                                                            (Stdlib.Int.succ
                                                            0) 0) :: (
    (xy (Stdlib.Int.succ (Stdlib.Int.succ 0)) 0) :: ((xy (Stdlib.Int.succ
                                                       (Stdlib.Int.succ
                                                       (Stdlib.Int.succ 0)))
                                                       0) :: ((xy
                                                                (Stdlib.Int.succ
                                                                (Stdlib.Int.succ
                                                                (Stdlib.Int.succ
                                                                0)))
                                                                (Stdlib.Int.succ
                                                                0)) :: []))))) :: ((
    (xy 0 (Stdlib.Int.succ 0)) :: ((xy (Stdlib.Int.succ 0) (Stdlib.Int.succ
                                     0)) :: ((xy (Stdlib.Int.succ
                                               (Stdlib.Int.succ 0))
                                               (Stdlib.Int.succ 0)) :: (
    (xy (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))) 0) :: (
    (xy (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))
      (Stdlib.Int.succ 0)) :: []))))) :: (((xy 0 (Stdlib.Int.succ
                                             (Stdlib.Int.succ
                                             (Stdlib.Int.succ 0)))) :: (
    (xy (Stdlib.Int.succ 0) 0) :: ((xy (Stdlib.Int.succ 0) (Stdlib.Int.succ
                                     0)) :: ((xy (Stdlib.Int.succ 0)
                                               (Stdlib.Int.succ
                                               (Stdlib.Int.succ 0))) :: (
    (xy (Stdlib.Int.succ 0) (Stdlib.Int.succ (Stdlib.Int.succ
      (Stdlib.Int.succ 0)))) :: []))))) :: [])))))))

(** val variants_D : variants **)

let variants_D =
  ((xy 0 0) :: ((xy 0 (Stdlib.Int.succ 0)) :: ((xy (Stdlib.Int.succ 0)
                                                 (Stdlib.Int.succ 0)) :: (
    (xy (Stdlib.Int.succ (Stdlib.Int.succ 0)) (Stdlib.Int.succ 0)) :: (
    (xy (Stdlib.Int.succ (Stdlib.Int.succ 0)) (Stdlib.Int.succ
      (Stdlib.Int.succ 0))) :: []))))) :: (((xy 0 0) :: ((xy (Stdlib.Int.succ
                                                           0) 0) :: (
    (xy (Stdlib.Int.succ 0) (Stdlib.Int.succ 0)) :: ((xy (Stdlib.Int.succ 0)
                                                       (Stdlib.Int.succ
                                                       (Stdlib.Int.succ 0))) :: (
    (xy (Stdlib.Int.succ (Stdlib.Int.succ 0)) (Stdlib.Int.succ
      (Stdlib.Int.succ 0))) :: []))))) :: (((xy 0 (Stdlib.Int.succ 0)) :: (
    (xy 0 (Stdlib.Int.succ (Stdlib.Int.succ 0))) :: ((xy (Stdlib.Int.succ 0)
                                                       (Stdlib.Int.succ 0)) :: (
    (xy (Stdlib.Int.succ (Stdlib.Int.succ 0)) 0) :: ((xy (Stdlib.Int.succ
                                                       (Stdlib.Int.succ 0))
                                                       (Stdlib.Int.succ 0)) :: []))))) :: ((
    (xy 0 (Stdlib.Int.succ (Stdlib.Int.succ 0))) :: ((xy (Stdlib.Int.succ 0)
                                                       0) :: ((xy
                                                                (Stdlib.Int.succ
                                                                0)
                                                                (Stdlib.Int.succ
                                                                0)) :: (
    (xy (Stdlib.Int.succ 0) (Stdlib.Int.succ (Stdlib.Int.succ 0))) :: (
    (xy (Stdlib.Int.succ (Stdlib.Int.succ 0)) 0) :: []))))) :: [])))

(** val variants_E : variants **)

let variants_E =
  ((xy 0 0) :: ((xy 0 (Stdlib.Int.succ 0)) :: ((xy 0 (Stdlib.Int.succ
                                                 (Stdlib.Int.succ 0))) :: (
    (xy (Stdlib.Int.succ 0) 0) :: ((xy (Stdlib.Int.succ (Stdlib.Int.succ 0))
                                     0) :: []))))) :: (((xy 0 0) :: (
    (xy 0 (Stdlib.Int.succ 0)) :: ((xy 0 (Stdlib.Int.succ (Stdlib.Int.succ
                                     0))) :: ((xy (Stdlib.Int.succ 0)
                                                (Stdlib.Int.succ
                                                (Stdlib.Int.succ 0))) :: (
    (xy (Stdlib.Int.succ (Stdlib.Int.succ 0)) (Stdlib.Int.succ
      (Stdlib.Int.succ 0))) :: []))))) :: (((xy 0 0) :: ((xy (Stdlib.Int.succ
                                                           0) 0) :: (
    (xy (Stdlib.Int.succ (Stdlib.Int.succ 0)) 0) :: ((xy (Stdlib.Int.succ
                                                       (Stdlib.Int.succ 0))
                                                       (Stdlib.Int.succ 0)) :: (
    (xy (Stdlib.Int.succ (Stdlib.Int.succ 0)) (Stdlib.Int.succ
      (Stdlib.Int.succ 0))) :: []))))) :: (((xy 0 (Stdlib.Int.succ
                                              (Stdlib.Int.succ 0))) :: (
    (xy (Stdlib.Int.succ 0) (Stdlib.Int.succ (Stdlib.Int.succ 0))) :: (
    (xy (Stdlib.Int.succ (Stdlib.Int.succ 0)) 0) :: ((xy (Stdlib.Int.succ
                                                       (Stdlib.Int.succ 0))
                                                       (Stdlib.Int.succ 0)) :: (
    (xy (Stdlib.Int.succ (Stdlib.Int.succ 0)) (Stdlib.Int.succ
      (Stdlib.Int.succ 0))) :: []))))) :: [])))

(** val variants_F : variants **)

let variants_F =
  ((xy 0 0) :: ((xy 0 (Stdlib.Int.succ 0)) :: ((xy 0 (Stdlib.Int.succ
                                                 (Stdlib.Int.succ 0))) :: (
    (xy 0 (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: (
    (xy (Stdlib.Int.succ 0) (Stdlib.Int.succ 0)) :: []))))) :: (((xy 0 0) :: (
    (xy 0 (Stdlib.Int.succ 0)) :: ((xy 0 (Stdlib.Int.succ (Stdlib.Int.succ
                                     0))) :: ((xy 0 (Stdlib.Int.succ
                                                (Stdlib.Int.succ
                                                (Stdlib.Int.succ 0)))) :: (
    (xy (Stdlib.Int.succ 0) (Stdlib.Int.succ (Stdlib.Int.succ 0))) :: []))))) :: ((
    (xy 0 0) :: ((xy (Stdlib.Int.succ 0) 0) :: ((xy (Stdlib.Int.succ 0)
                                                  (Stdlib.Int.succ 0)) :: (
    (xy (Stdlib.Int.succ (Stdlib.Int.succ 0)) 0) :: ((xy (Stdlib.Int.succ
                                                       (Stdlib.Int.succ
                                                       (Stdlib.Int.succ 0)))
                                                       0) :: []))))) :: ((
    (xy 0 0) :: ((xy (Stdlib.Int.succ 0) 0) :: ((xy (Stdlib.Int.succ
                                                  (Stdlib.Int.succ 0)) 0) :: (
    (xy (Stdlib.Int.succ (Stdlib.Int.succ 0)) (Stdlib.Int.succ 0)) :: (
    (xy (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))) 0) :: []))))) :: ((
    (xy 0 (Stdlib.Int.succ 0)) :: ((xy (Stdlib.Int.succ 0) 0) :: ((xy
                                                                    (Stdlib.Int.succ
                                                                    0)
                                                                    (Stdlib.Int.succ
                                                                    0)) :: (
    (xy (Stdlib.Int.succ 0) (Stdlib.Int.succ (Stdlib.Int.succ 0))) :: (
    (xy (Stdlib.Int.succ 0) (Stdlib.Int.succ (Stdlib.Int.succ
      (Stdlib.Int.succ 0)))) :: []))))) :: (((xy 0 (Stdlib.Int.succ 0)) :: (
    (xy (Stdlib.Int.succ 0) 0) :: ((xy (Stdlib.Int.succ 0) (Stdlib.Int.succ
                                     0)) :: ((xy (Stdlib.Int.succ
                                               (Stdlib.Int.succ 0))
                                               (Stdlib.Int.succ 0)) :: (
    (xy (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))
      (Stdlib.Int.succ 0)) :: []))))) :: (((xy 0 (Stdlib.Int.succ 0)) :: (
    (xy (Stdlib.Int.succ 0) (Stdlib.Int.succ 0)) :: ((xy (Stdlib.Int.succ
                                                       (Stdlib.Int.succ 0)) 0) :: (
    (xy (Stdlib.Int.succ (Stdlib.Int.succ 0)) (Stdlib.Int.succ 0)) :: (
    (xy (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))
      (Stdlib.Int.succ 0)) :: []))))) :: (((xy 0 (Stdlib.Int.succ
                                             (Stdlib.Int.succ 0))) :: (
    (xy (Stdlib.Int.succ 0) 0) :: ((xy (Stdlib.Int.succ 0) (Stdlib.Int.succ
                                     0)) :: ((xy (Stdlib.Int.succ 0)
                                               (Stdlib.Int.succ
                                               (Stdlib.Int.succ 0))) :: (
    (xy (Stdlib.Int.succ 0) (Stdlib.Int.succ (Stdlib.Int.succ
      (Stdlib.Int.succ 0)))) :: []))))) :: [])))))))

(** val variants_G : variants **)

let variants_G =
  ((xy 0 0) :: ((xy 0 (Stdlib.Int.succ 0)) :: ((xy 0 (Stdlib.Int.succ
                                                 (Stdlib.Int.succ 0))) :: (
    (xy (Stdlib.Int.succ 0) 0) :: ((xy (Stdlib.Int.succ 0) (Stdlib.Int.succ
                                     (Stdlib.Int.succ 0))) :: []))))) :: ((
    (xy 0 0) :: ((xy 0 (Stdlib.Int.succ 0)) :: ((xy (Stdlib.Int.succ 0) 0) :: (
    (xy (Stdlib.Int.succ (Stdlib.Int.succ 0)) 0) :: ((xy (Stdlib.Int.succ
                                                       (Stdlib.Int.succ 0))
                                                       (Stdlib.Int.succ 0)) :: []))))) :: ((
    (xy 0 0) :: ((xy 0 (Stdlib.Int.succ 0)) :: ((xy (Stdlib.Int.succ 0)
                                                  (Stdlib.Int.succ 0)) :: (
    (xy (Stdlib.Int.succ (Stdlib.Int.succ 0)) 0) :: ((xy (Stdlib.Int.succ
                                                       (Stdlib.Int.succ 0))
                                                       (Stdlib.Int.succ 0)) :: []))))) :: ((
    (xy 0 0) :: ((xy 0 (Stdlib.Int.succ (Stdlib.Int.succ 0))) :: ((xy
                                                                    (Stdlib.Int.succ
                                                                    0) 0) :: (
    (xy (Stdlib.Int.succ 0) (Stdlib.Int.succ 0)) :: ((xy (Stdlib.Int.succ 0)
                                                       (Stdlib.Int.succ
                                                       (Stdlib.Int.succ 0))) :: []))))) :: [])))

(** val variants_H : variants **)

let variants_H =
  ((xy 0 0) :: ((xy 0 (Stdlib.Int.succ 0)) :: ((xy 0 (Stdlib.Int.succ
                                                 (Stdlib.Int.succ 0))) :: (
    (xy (Stdlib.Int.succ 0) 0) :: ((xy (Stdlib.Int.succ 0) (Stdlib.Int.succ
                                     0)) :: []))))) :: (((xy 0 0) :: (
    (xy 0 (Stdlib.Int.succ 0)) :: ((xy 0 (Stdlib.Int.succ (Stdlib.Int.succ
                                     0))) :: ((xy (Stdlib.Int.succ 0)
                                                (Stdlib.Int.succ 0)) :: (
    (xy (Stdlib.Int.succ 0) (Stdlib.Int.succ (Stdlib.Int.succ 0))) :: []))))) :: ((
    (xy 0 0) :: ((xy 0 (Stdlib.Int.succ 0)) :: ((xy (Stdlib.Int.succ 0) 0) :: (
    (xy (Stdlib.Int.succ 0) (Stdlib.Int.succ 0)) :: ((xy (Stdlib.Int.succ 0)
                                                       (Stdlib.Int.succ
                                                       (Stdlib.Int.succ 0))) :: []))))) :: ((
    (xy 0 0) :: ((xy 0 (Stdlib.Int.succ 0)) :: ((xy (Stdlib.Int.succ 0) 0) :: (
    (xy (Stdlib.Int.succ 0) (Stdlib.Int.succ 0)) :: ((xy (Stdlib.Int.succ
                                                       (Stdlib.Int.succ 0)) 0) :: []))))) :: ((
    (xy 0 0) :: ((xy 0 (Stdlib.Int.succ 0)) :: ((xy (Stdlib.Int.succ 0) 0) :: (
    (xy (Stdlib.Int.succ 0) (Stdlib.Int.succ 0)) :: ((xy (Stdlib.Int.succ
                                                       (Stdlib.Int.succ 0))
                                                       (Stdlib.Int.succ 0)) :: []))))) :: ((
    (xy 0 0) :: ((xy (Stdlib.Int.succ 0) 0) :: ((xy (Stdlib.Int.succ 0)
                                                  (Stdlib.Int.succ 0)) :: (
    (xy (Stdlib.Int.succ (Stdlib.Int.succ 0)) 0) :: ((xy (Stdlib.Int.succ
                                                       (Stdlib.Int.succ 0))
                                                       (Stdlib.Int.succ 0)) :: []))))) :: ((
    (xy 0 (Stdlib.Int.succ 0)) :: ((xy 0 (Stdlib.Int.succ (Stdlib.Int.succ
                                     0))) :: ((xy (Stdlib.Int.succ 0) 0) :: (
    (xy (Stdlib.Int.succ 0) (Stdlib.Int.succ 0)) :: ((xy (Stdlib.Int.succ 0)
                                                       (Stdlib.Int.succ
                                                       (Stdlib.Int.succ 0))) :: []))))) :: ((
    (xy 0 (Stdlib.Int.succ 0)) :: ((xy (Stdlib.Int.succ 0) 0) :: ((xy
                                                                    (Stdlib.Int.succ
                                                                    0)
                                                                    (Stdlib.Int.succ
                                                                    0)) :: (
    (xy (Stdlib.Int.succ (Stdlib.Int.succ 0)) 0) :: ((xy (Stdlib.Int.succ
                                                       (Stdlib.Int.succ 0))
                                                       (Stdlib.Int.succ 0)) :: []))))) :: [])))))))

(** val variants_of : piece_id -> variants **)

let variants_of = function
| A -> variants_A
| B -> variants_B
| C -> variants_C
| D -> variants_D
| E -> variants_E
| F -> variants_F
| G -> variants_G
| H -> variants_H

(** val translate : int -> int -> shape -> cell list **)

let translate r c s =
  map (fun q -> xy (add r (fst q)) (add c (snd q))) s

(** val placement_cells : placement -> cell list option **)

let placement_cells pl =
  match nth_error (variants_of (placement_piece pl))
          (placement_orientation pl) with
  | Some s -> Some (translate (placement_row pl) (placement_col pl) s)
  | None -> None

(** val collect_cells : placement list -> cell list option **)

let rec collect_cells = function
| [] -> Some []
| pl :: rest ->
  (match placement_cells pl with
   | Some xs ->
     (match collect_cells rest with
      | Some ys -> Some (app xs ys)
      | None -> None)
   | None -> None)

(** val nodupb : ('a1 -> 'a1 -> bool) -> 'a1 list -> bool **)

let rec nodupb eqb0 = function
| [] -> true
| x :: rest -> (&&) (negb (existsb (eqb0 x) rest)) (nodupb eqb0 rest)

(** val in_boardb : cell -> bool **)

let in_boardb c =
  existsb (cell_eqb c) board_cells

(** val pieces_presentb : piece_id list -> bool **)

let pieces_presentb ids =
  forallb (fun p -> existsb (piece_eqb p) ids) all_pieces

(** val verify : int -> int -> placement list -> bool **)

let verify month day cert =
  match target_cells month day with
  | Some p ->
    let (mc, dc) = p in
    (match collect_cells cert with
     | Some cells ->
       (&&)
         ((&&)
           ((&&)
             ((&&)
               ((&&)
                 ((&&)
                   ((&&)
                     ((=) (length cert) (Stdlib.Int.succ (Stdlib.Int.succ
                       (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                       (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                       0)))))))))
                     (nodupb piece_eqb (map placement_piece cert)))
                   (pieces_presentb (map placement_piece cert)))
                 ((=) (length cells) (Stdlib.Int.succ (Stdlib.Int.succ
                   (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                   (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                   (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                   (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                   (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                   (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                   (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                   (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                   (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                   (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                   (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                   (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                   (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
                   0)))))))))))))))))))))))))))))))))))))))))))
               (nodupb cell_eqb cells))
             (forallb in_boardb cells))
           (negb (existsb (cell_eqb mc) cells)))
         (negb (existsb (cell_eqb dc) cells))
     | None -> false)
  | None -> false

(** val solution_table : ((int * int) * placement list) list **)

let solution_table =
  (((Stdlib.Int.succ 0), (Stdlib.Int.succ 0)), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0,
    0)) :: ((Place (B, 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (D, 0, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (E,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0)) :: ((Place (F,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    0)) :: ((Place (G, 0, (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: []))))))))) :: ((((Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0,
    0)) :: ((Place (B, 0, (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), 0)) :: ((Place (D, 0, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (E,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (F, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0)) :: ((Place (G,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), 0)) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: []))))))))) :: ((((Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))), ((Place (A,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), 0)) :: ((Place (B, (Stdlib.Int.succ 0), 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), 0)) :: ((Place (D, 0, (Stdlib.Int.succ 0),
    0)) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    0))) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place
    (G, (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))) :: ((Place (H, (Stdlib.Int.succ 0), 0,
    (Stdlib.Int.succ 0))) :: []))))))))) :: ((((Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place (B, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))) :: ((Place (C, (Stdlib.Int.succ 0), 0,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (D, (Stdlib.Int.succ
    0), (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (E, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (F, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0,
    0)) :: ((Place (G, 0, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    0)) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    0))) :: []))))))))) :: ((((Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))), ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place (B, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ 0))) :: ((Place (C, (Stdlib.Int.succ 0), 0,
    (Stdlib.Int.succ 0))) :: ((Place (D, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0)) :: ((Place (F, (Stdlib.Int.succ 0), (Stdlib.Int.succ 0),
    0)) :: ((Place (G, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    0)) :: []))))))))) :: ((((Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))), ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0, 0)) :: ((Place (B, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (D,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    0)) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0)) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ 0),
    (Stdlib.Int.succ 0))) :: ((Place (G, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ 0))) :: ((Place (H,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: []))))))))) :: ((((Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))) :: ((Place (B, 0, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0, (Stdlib.Int.succ
    0))) :: ((Place (D, 0, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (E,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    0)) :: ((Place (G, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ 0))) :: ((Place (H, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ 0), 0)) :: []))))))))) :: ((((Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))))), ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place
    (B, 0, 0, (Stdlib.Int.succ 0))) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ 0), 0)) :: ((Place (D, 0, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (E,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    0), (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (F,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ (Stdlib.Int.succ
    0)))) :: ((Place (G, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))) :: ((Place (H, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))),
    (Stdlib.Int.succ 0))) :: []))))))))) :: ((((Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))))))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), 0)) :: ((Place (B, 0, 0, (Stdlib.Int.succ 0))) :: ((Place (C,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ 0), 0)) :: ((Place (D, 0, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (E,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    0), (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (F,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ (Stdlib.Int.succ
    0)))) :: ((Place (G, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    0)) :: []))))))))) :: ((((Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))))))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), 0)) :: ((Place (B, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ 0), 0)) :: ((Place (D, (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ 0))) :: ((Place (E, (Stdlib.Int.succ 0), 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (F,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (G, (Stdlib.Int.succ (Stdlib.Int.succ
    0)), 0, (Stdlib.Int.succ 0))) :: ((Place (H, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))) :: []))))))))) :: ((((Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))), ((Place (A,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), 0)) :: ((Place (B, 0, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (C, (Stdlib.Int.succ
    0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ 0))) :: ((Place (D, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (E, (Stdlib.Int.succ
    0), (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (F,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    0)) :: ((Place (G, (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), 0)) :: ((Place (H, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))), 0, 0)) :: []))))))))) :: ((((Stdlib.Int.succ
    0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))))))))), ((Place (A, 0, 0, (Stdlib.Int.succ 0))) :: ((Place (B,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ 0))) :: ((Place (D, 0, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (E,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0)) :: ((Place (F,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ 0),
    (Stdlib.Int.succ 0))) :: ((Place (G, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ 0),
    0)) :: []))))))))) :: ((((Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))))))))))), ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place
    (B, 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (C, (Stdlib.Int.succ
    0), 0, (Stdlib.Int.succ 0))) :: ((Place (D, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0)) :: ((Place (E,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0)) :: ((Place (F, 0,
    (Stdlib.Int.succ 0), 0)) :: ((Place (G, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ 0))) :: ((Place (H,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ
    0)))) :: []))))))))) :: ((((Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))))))))))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), 0, 0)) :: ((Place (B, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (C,
    0, (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    0))) :: ((Place (D, 0, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (E, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (F, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ 0))) :: ((Place (G,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), 0)) :: ((Place (H, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), 0, (Stdlib.Int.succ (Stdlib.Int.succ
    0)))) :: []))))))))) :: ((((Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))))))), ((Place (A,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place (B, 0, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (C,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ 0))) :: ((Place
    (D, 0, 0, (Stdlib.Int.succ 0))) :: ((Place (E, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (F, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (G, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ 0), 0)) :: []))))))))) :: ((((Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))))))))))))), ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))), 0)) :: ((Place
    (B, 0, (Stdlib.Int.succ 0), 0)) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))), 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (D, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: ((Place (E, (Stdlib.Int.succ 0), 0,
    (Stdlib.Int.succ 0))) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place
    (G, (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), 0)) :: ((Place (H, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))) :: []))))))))) :: ((((Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))))))))))))))), ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ 0))) :: ((Place (B, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place (D,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (E,
    0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (F,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    0)) :: ((Place (G, 0, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))), 0, 0)) :: []))))))))) :: ((((Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))))))))))))))), ((Place (A,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (B, (Stdlib.Int.succ
    0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))) :: ((Place (C, (Stdlib.Int.succ 0), 0,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (D, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place
    (E, (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0)) :: ((Place
    (F, (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (G, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))), 0, 0)) :: []))))))))) :: ((((Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))))))))))),
    ((Place (A, (Stdlib.Int.succ 0), (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (B, (Stdlib.Int.succ
    0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))) :: ((Place (C, (Stdlib.Int.succ 0), 0,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (D, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place
    (E, (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0)) :: ((Place
    (F, (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (G, (Stdlib.Int.succ
    0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (H,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))), 0,
    0)) :: []))))))))) :: ((((Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))))))))))))))))), ((Place (A, (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (B, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), 0)) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))) :: ((Place (D, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ 0))) :: ((Place (E, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), 0)) :: ((Place (F, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (G, 0, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))), 0, 0)) :: []))))))))) :: ((((Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))))))))))))))))))), ((Place (A, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (B,
    (Stdlib.Int.succ 0), 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (D, (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), 0)) :: ((Place (E, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0, (Stdlib.Int.succ
    0))) :: ((Place (F, 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))), (Stdlib.Int.succ
    0))) :: ((Place (G, (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0)) :: ((Place
    (H, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))), 0,
    0)) :: []))))))))) :: ((((Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))))))))))))))))))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), 0)) :: ((Place (B, (Stdlib.Int.succ 0), 0, (Stdlib.Int.succ
    0))) :: ((Place (C, 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ 0))) :: ((Place (D,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (E, (Stdlib.Int.succ
    0), 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ 0), 0)) :: ((Place (G, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))) :: []))))))))) :: ((((Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))))))))))))))))))))), ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), (Stdlib.Int.succ 0))) :: ((Place (B, 0, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))) :: ((Place (D, 0, 0, (Stdlib.Int.succ
    0))) :: ((Place (E, 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), 0)) :: ((Place (F, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    0))) :: ((Place (G, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (H, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ 0),
    0)) :: []))))))))) :: ((((Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))))))))))))))))))))), ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0)) :: ((Place (B, 0, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (C,
    0, (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (D, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ 0))) :: ((Place
    (E, (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0)) :: ((Place
    (F, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (G, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))), 0, 0)) :: []))))))))) :: ((((Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))))))))))))))))))))))), ((Place (A,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place (B,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ 0), 0)) :: ((Place (D,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ 0))) :: ((Place
    (E, (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place
    (F, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))),
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (G, (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0,
    (Stdlib.Int.succ 0))) :: ((Place (H, 0, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: []))))))))) :: ((((Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))))))))))))))))))))))), ((Place (A,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (B, (Stdlib.Int.succ
    0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))) :: ((Place (C, 0, 0, (Stdlib.Int.succ
    0))) :: ((Place (D, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (E, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), 0)) :: ((Place (F, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ 0))) :: ((Place (G, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ 0),
    0)) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), 0)) :: []))))))))) :: ((((Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))))))))))))))))))))))))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ 0),
    0)) :: ((Place (B, 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))) :: ((Place (D, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0)) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ 0))) :: ((Place (G, 0, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (H, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0, (Stdlib.Int.succ
    0))) :: []))))))))) :: ((((Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))))))))))))))))))))))))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), 0)) :: ((Place (B, 0, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ 0), 0)) :: ((Place (D,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ 0))) :: ((Place
    (E, (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (G, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), 0, (Stdlib.Int.succ 0))) :: ((Place (H,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: []))))))))) :: ((((Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))))))))))))))))))))))))))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), 0)) :: ((Place (B, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ 0))) :: ((Place
    (C, 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: ((Place (D, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (E, 0, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place
    (G, (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))), 0, 0)) :: []))))))))) :: ((((Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))))))))))))))))))))))))))),
    ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place (B, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))) :: ((Place (C, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), 0)) :: ((Place (D, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))) :: ((Place (E, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ 0))) :: ((Place (G, 0, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (H, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))), 0, 0)) :: []))))))))) :: ((((Stdlib.Int.succ
    0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))))))))))))))))))))))))))))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    0)) :: ((Place (B, (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place (C, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), (Stdlib.Int.succ 0))) :: ((Place (D, 0, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ
    0)), 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (G, 0, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    0)) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))), 0,
    0)) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ 0)), ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    0, 0)) :: ((Place (B, 0, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    0)))) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place (D,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (E, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (F, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))) :: ((Place (G, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ 0))) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ 0))), ((Place (A,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0, (Stdlib.Int.succ 0))) :: ((Place (B, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place
    (C, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), 0, 0)) :: ((Place (D, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (E, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), 0)) :: ((Place (F, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (G, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: ((Place (H, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ 0))) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), 0, 0)) :: ((Place (B, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))), 0)) :: ((Place
    (C, (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (D, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (E,
    (Stdlib.Int.succ 0), 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0, (Stdlib.Int.succ
    0))) :: ((Place (G, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    0)) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ
    0)), 0, 0)) :: ((Place (B, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))), 0)) :: ((Place
    (C, 0, 0, (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (D,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (E, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    0)) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place (G,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (H,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))), ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ
    0)), 0, 0)) :: ((Place (B, (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place (C,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (D, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (E, 0, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    0)) :: ((Place (G, 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (H,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), 0)) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), 0, 0)) :: ((Place (B, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), 0)) :: ((Place (C, (Stdlib.Int.succ 0), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: ((Place (D, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0)) :: ((Place (F,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (G, 0,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place
    (H, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))), ((Place (A,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0, 0)) :: ((Place (B, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), 0)) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place (D, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0)) :: ((Place (F,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (G, (Stdlib.Int.succ 0), 0,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (H, (Stdlib.Int.succ
    0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0, 0)) :: ((Place (B, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ 0))) :: ((Place (C, (Stdlib.Int.succ 0), 0,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (D, 0,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ 0))) :: ((Place (E,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0)) :: ((Place (F,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place (G,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))))))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))) :: ((Place (B, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))), 0)) :: ((Place
    (C, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0, 0)) :: ((Place (D, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))) :: ((Place (E, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place
    (F, (Stdlib.Int.succ 0), 0, (Stdlib.Int.succ (Stdlib.Int.succ
    0)))) :: ((Place (G, (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), 0)) :: ((Place (H, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))))))), ((Place (A,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0, 0)) :: ((Place (B, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), 0)) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (D, 0, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0)) :: ((Place (F,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))) :: ((Place (G, (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))) :: ((Place (H, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), 0, (Stdlib.Int.succ (Stdlib.Int.succ
    0)))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))), ((Place
    (A, (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0, 0)) :: ((Place (B, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), 0)) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (D, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ
    0)))) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0)) :: ((Place (F,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))) :: ((Place (G, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))) :: ((Place (H, 0, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))))))))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0, 0)) :: ((Place (B, 0, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (C,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (D, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place
    (E, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (F, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), 0, (Stdlib.Int.succ 0))) :: ((Place (G, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), 0)) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))))), ((Place (A,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0, 0)) :: ((Place (B, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), 0)) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (D,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (E,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    0)), 0)) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), 0, (Stdlib.Int.succ (Stdlib.Int.succ
    0)))) :: ((Place (G, 0, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))))))))))), ((Place (A,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0,
    0)) :: ((Place (B, 0, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (D, (Stdlib.Int.succ
    0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    0)) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (F,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), 0, (Stdlib.Int.succ 0))) :: ((Place (G,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))) :: ((Place (H, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))),
    0)) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))))))),
    ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (B, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))), 0)) :: ((Place
    (C, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0, 0)) :: ((Place (D, (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ 0))) :: ((Place
    (E, (Stdlib.Int.succ 0), 0, (Stdlib.Int.succ (Stdlib.Int.succ
    0)))) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place (G, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0)) :: ((Place (H,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))))))))))))), ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0,
    0)) :: ((Place (B, (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), 0)) :: ((Place (D, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), 0)) :: ((Place (F, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: ((Place (G, 0, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (H, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))))))))), ((Place (A,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    0))) :: ((Place (B, 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))), 0)) :: ((Place (C,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0, 0)) :: ((Place (D, (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (E, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (F, (Stdlib.Int.succ 0), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: ((Place (G, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0)) :: ((Place (H,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))))))))))))))), ((Place (A,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0, 0)) :: ((Place (B, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), 0)) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (D,
    0, (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (E, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    0)) :: ((Place (F, (Stdlib.Int.succ 0), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: ((Place (G, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place
    (H, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))))))))))),
    ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0,
    0)) :: ((Place (B, 0, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))), (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))) :: ((Place (D, (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place (E,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (F,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), 0, (Stdlib.Int.succ 0))) :: ((Place (G,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), 0)) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))))))))))))))))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), 0, 0)) :: ((Place (B, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), 0)) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (D, (Stdlib.Int.succ
    0), (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (E, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    0)) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), 0, (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (G, 0, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (H,
    0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))))))))))))))))))), ((Place (A, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (B,
    0, 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ 0))) :: ((Place
    (D, (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    0)) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0)) :: ((Place (F, (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ 0))) :: ((Place (G,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0,
    0)) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))))))))))))))))))), ((Place (A,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), 0)) :: ((Place (B, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))), 0, 0)) :: ((Place (D,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (E, (Stdlib.Int.succ
    0), 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (F, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0)) :: ((Place (G,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))), 0, (Stdlib.Int.succ
    0))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))))))))))))))))))))), ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), (Stdlib.Int.succ 0))) :: ((Place (B, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))), (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))) :: ((Place (D, (Stdlib.Int.succ 0), 0, 0)) :: ((Place (E, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0)) :: ((Place (F, (Stdlib.Int.succ 0), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: ((Place (G, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (H, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    0)) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))))))))))))))))))))), ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))), 0, (Stdlib.Int.succ 0))) :: ((Place (B, (Stdlib.Int.succ 0), 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))), 0,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (D, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0)) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0,
    0)) :: ((Place (G, 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ 0))) :: ((Place (H,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))))))))))))))))))))))), ((Place (A,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0, 0)) :: ((Place (B, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (D,
    (Stdlib.Int.succ 0), 0, (Stdlib.Int.succ (Stdlib.Int.succ
    0)))) :: ((Place (E, (Stdlib.Int.succ 0), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (F, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    0)), 0)) :: ((Place (G, (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place
    (H, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), 0)) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))))))))))))))))))))))), ((Place
    (A, (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0, 0)) :: ((Place (B, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), 0)) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (D, 0, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0)) :: ((Place (F,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (G, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))) :: ((Place (H, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))))))))))))))))))))))))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), 0)) :: ((Place (B, 0, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))) :: ((Place (D, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), 0)) :: ((Place (E, 0, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    0)) :: ((Place (F, (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: ((Place (G, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), 0, 0)) :: ((Place (H, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))))))))))))))))))))))))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0, 0)) :: ((Place (B, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), 0)) :: ((Place (C, (Stdlib.Int.succ 0), 0,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (D, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (F,
    0, (Stdlib.Int.succ 0), (Stdlib.Int.succ 0))) :: ((Place (G,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place (H,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))))))))))))))))))))))))))), ((Place (A,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0, 0)) :: ((Place (B, 0, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0)) :: ((Place (D, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: ((Place (E, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    0)))) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place (G,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ 0))) :: ((Place
    (H, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ 0)), ((Place (A, (Stdlib.Int.succ
    0), (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    0))) :: ((Place (B, (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ 0))) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    0)) :: ((Place (D, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (E,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (F, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (G, 0, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ
    0)), 0, 0)) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ 0))), ((Place
    (A, (Stdlib.Int.succ 0), 0, 0)) :: ((Place (B, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0)) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (D, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ (Stdlib.Int.succ
    0)))) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ 0), 0)) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place
    (G, (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place
    (H, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), 0, 0)) :: ((Place (B, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (C,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0, (Stdlib.Int.succ 0))) :: ((Place (D, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place
    (E, (Stdlib.Int.succ 0), 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    0)))) :: ((Place (G, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (H,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))),
    0)) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), ((Place (A, (Stdlib.Int.succ 0), 0,
    0)) :: ((Place (B, (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ 0))) :: ((Place
    (C, (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place (D,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ 0), 0)) :: ((Place (E,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (G, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (H, (Stdlib.Int.succ
    0), 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))), ((Place (A, (Stdlib.Int.succ
    0), 0, 0)) :: ((Place (B, (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place (C,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place (D,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ 0), 0)) :: ((Place (E,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0)) :: ((Place
    (F, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (G, (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (H, (Stdlib.Int.succ 0), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))), ((Place (A,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (B, (Stdlib.Int.succ 0), 0, 0)) :: ((Place (C,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place (D, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place
    (E, (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0)) :: ((Place
    (F, (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (G, 0, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (H,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))))), ((Place (A, (Stdlib.Int.succ 0), 0, 0)) :: ((Place (B, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), 0)) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (D,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    0), (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (E,
    (Stdlib.Int.succ 0), 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ 0), 0)) :: ((Place (G, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ 0))) :: ((Place (H, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))))), ((Place (A, (Stdlib.Int.succ 0), 0,
    0)) :: ((Place (B, 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))), 0)) :: ((Place (C,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place (D,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ 0), 0)) :: ((Place (E,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (F,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    0)) :: ((Place (G, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (H, (Stdlib.Int.succ
    0), 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))), ((Place (A,
    (Stdlib.Int.succ 0), 0, 0)) :: ((Place (B, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ 0),
    0)) :: ((Place (D, 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (E, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (F, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), 0)) :: ((Place (G, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ 0))) :: ((Place (H,
    (Stdlib.Int.succ 0), 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))))))), ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), 0, 0)) :: ((Place (B, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: ((Place (D, 0, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (E,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place
    (G, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0,
    (Stdlib.Int.succ 0))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    0))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))))))))), ((Place (A, (Stdlib.Int.succ 0), 0, 0)) :: ((Place (B, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), 0)) :: ((Place (C, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0)) :: ((Place (D, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: ((Place (E, (Stdlib.Int.succ 0), 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (F,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place (G,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))), (Stdlib.Int.succ 0),
    0)) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))))))))), ((Place (A, (Stdlib.Int.succ 0), 0,
    0)) :: ((Place (B, 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))), 0)) :: ((Place (C, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (D, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ 0), 0)) :: ((Place (G, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ 0))) :: ((Place (H, (Stdlib.Int.succ 0), 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))))), ((Place (A,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))) :: ((Place (B, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place
    (D, (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    0)))) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0)) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0)) :: ((Place (G,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (H, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0,
    0)) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))))))))))),
    ((Place (A, (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (B, (Stdlib.Int.succ
    0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0)) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0,
    0)) :: ((Place (D, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (E, 0, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (G,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))) :: ((Place (H, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ 0),
    (Stdlib.Int.succ 0))) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))))))))))))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), 0)) :: ((Place (B, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place
    (C, (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0, 0)) :: ((Place (D,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ 0))) :: ((Place
    (E, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place
    (F, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (G, 0, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (H, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ 0), (Stdlib.Int.succ
    0))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))))))))))))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), 0)) :: ((Place (B, (Stdlib.Int.succ 0), 0, 0)) :: ((Place (C,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place (D, 0,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (E, (Stdlib.Int.succ 0), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (F, (Stdlib.Int.succ
    0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (G,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), 0)) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))),
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ
    0)))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))))))))), ((Place (A,
    (Stdlib.Int.succ 0), 0, 0)) :: ((Place (B, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))), (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))) :: ((Place (D, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: ((Place (E, (Stdlib.Int.succ 0), 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (F,
    0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), (Stdlib.Int.succ 0))) :: ((Place (G,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), 0)) :: ((Place (H, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), (Stdlib.Int.succ 0), 0)) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))))))))))))))), ((Place (A, (Stdlib.Int.succ 0), 0, 0)) :: ((Place
    (B, (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), 0)) :: ((Place (C, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (D, (Stdlib.Int.succ 0), (Stdlib.Int.succ 0),
    0)) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0)) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (G, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (H,
    (Stdlib.Int.succ 0), 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))))))))))))))))), ((Place (A, (Stdlib.Int.succ 0),
    0, 0)) :: ((Place (B, (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ 0))) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    0)) :: ((Place (D, (Stdlib.Int.succ 0), (Stdlib.Int.succ 0),
    0)) :: ((Place (E, (Stdlib.Int.succ 0), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (F, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))) :: ((Place (G, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))))))))))))))))), ((Place (A, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (B,
    (Stdlib.Int.succ 0), 0, 0)) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))) :: ((Place (D, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place (E,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0)) :: ((Place (F,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (G, 0, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))),
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ
    0)))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))))))))))))), ((Place (A, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (B, (Stdlib.Int.succ 0), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (C,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (D, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0)) :: ((Place (E,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0,
    (Stdlib.Int.succ 0))) :: ((Place (F, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))),
    (Stdlib.Int.succ 0))) :: ((Place (G, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0)) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0,
    0)) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))))))))))))))))))), ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))), 0)) :: ((Place
    (B, (Stdlib.Int.succ 0), 0, 0)) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), 0)) :: ((Place (D, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (E, (Stdlib.Int.succ 0), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (F, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0)) :: ((Place (G, 0, (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))))))))))))))))))))), ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), (Stdlib.Int.succ 0))) :: ((Place (B, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place
    (D, 0, 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ
    0)), 0, 0)) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ 0))) :: ((Place
    (G, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0,
    (Stdlib.Int.succ 0))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))))))))))))))))))))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    0)) :: ((Place (B, 0, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (C, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (D, 0, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ 0))) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0)) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (G, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (H,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0,
    0)) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))))))))))))))))), ((Place
    (A, (Stdlib.Int.succ 0), 0, 0)) :: ((Place (B, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), 0)) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))), (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))) :: ((Place (D, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: ((Place (E, (Stdlib.Int.succ 0), 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (F,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ 0),
    0)) :: ((Place (G, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ 0))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))))))))))))))))))))))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (B,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), 0, 0)) :: ((Place (D, 0, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (E,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0)) :: ((Place (F,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))), 0,
    (Stdlib.Int.succ 0))) :: ((Place (G, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ 0))) :: ((Place (H, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    0))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))))))))))))))))))))))))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (B, (Stdlib.Int.succ 0), 0, 0)) :: ((Place (C,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place (D, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place
    (E, (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0)) :: ((Place
    (F, (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (G, 0, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (H,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))))))))))))))))))))))))), ((Place (A,
    (Stdlib.Int.succ 0), 0, 0)) :: ((Place (B, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ 0))) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), 0)) :: ((Place (D, (Stdlib.Int.succ 0), (Stdlib.Int.succ 0),
    0)) :: ((Place (E, (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (G, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (H, (Stdlib.Int.succ 0), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))))))))))))))))))))),
    ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place
    (B, (Stdlib.Int.succ 0), 0, 0)) :: ((Place (C, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (D,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ 0),
    (Stdlib.Int.succ 0))) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ 0))) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place
    (G, (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (H, (Stdlib.Int.succ 0), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))))))))))))))))))))))))))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    0)) :: ((Place (B, 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), 0, 0)) :: ((Place (D, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))) :: ((Place (E, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ 0))) :: ((Place (G, 0, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (H, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ 0), (Stdlib.Int.succ
    0))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))))))))))))))))))))))))))))), ((Place (A, (Stdlib.Int.succ 0), 0,
    0)) :: ((Place (B, 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place
    (D, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place
    (E, (Stdlib.Int.succ 0), 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ 0))) :: ((Place (G, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0)) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))),
    (Stdlib.Int.succ 0), 0)) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ 0)), ((Place (A, 0, 0, 0)) :: ((Place (B, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), 0)) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place
    (D, 0, (Stdlib.Int.succ 0), 0)) :: ((Place (E, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place
    (G, (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), ((Place (A, 0, 0, 0)) :: ((Place (B,
    (Stdlib.Int.succ 0), 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (D, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ 0))) :: ((Place
    (E, (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0)) :: ((Place
    (F, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ 0))) :: ((Place (G, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ 0), 0)) :: ((Place (H, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))), ((Place (A, 0,
    0, 0)) :: ((Place (B, (Stdlib.Int.succ 0), (Stdlib.Int.succ 0),
    0)) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place
    (D, (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (E, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (F,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ 0))) :: ((Place
    (G, (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0)) :: ((Place (H, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))), ((Place (A,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (B,
    0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), 0)) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place (D, 0, 0,
    0)) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ 0), 0)) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place
    (G, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (H, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))), ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (B, (Stdlib.Int.succ
    0), (Stdlib.Int.succ 0), (Stdlib.Int.succ 0))) :: ((Place (C,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (D, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place
    (E, 0, 0, 0)) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place
    (G, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (H,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))),
    0)) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))), ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))),
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place
    (B, 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (C,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place (D,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ 0),
    0)) :: ((Place (E, 0, 0, 0)) :: ((Place (F, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), (Stdlib.Int.succ 0))) :: ((Place (G, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0)) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: ((Place (B, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), 0)) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place (D,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place
    (E, 0, 0, 0)) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (G, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (H,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), (Stdlib.Int.succ 0), (Stdlib.Int.succ
    0))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))))), ((Place
    (A, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))), 0,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (B, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), 0)) :: ((Place (C, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0)) :: ((Place (D, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (E, 0, 0,
    0)) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place
    (G, (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))),
    (Stdlib.Int.succ 0), (Stdlib.Int.succ
    0))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))))))), ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))), 0, (Stdlib.Int.succ (Stdlib.Int.succ
    0)))) :: ((Place (B, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))), 0)) :: ((Place
    (C, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place (D,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ 0), (Stdlib.Int.succ
    0))) :: ((Place (E, 0, 0, 0)) :: ((Place (F, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (G, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), 0)) :: ((Place (H, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))))))), ((Place (A,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))), 0,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (B, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ 0))) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), 0, 0)) :: ((Place (D, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (E, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), 0)) :: ((Place (F, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))) :: ((Place (G, (Stdlib.Int.succ 0), 0, (Stdlib.Int.succ
    0))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))), ((Place (A,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))), 0,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (B, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))), 0, 0)) :: ((Place (D,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place (E,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))), (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (G, (Stdlib.Int.succ 0), 0,
    (Stdlib.Int.succ 0))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))),
    0)) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))))))))),
    ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))), 0, (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (B, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))), 0, 0)) :: ((Place (D,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place
    (E, (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0)) :: ((Place
    (F, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))) :: ((Place (G, (Stdlib.Int.succ 0), 0,
    (Stdlib.Int.succ 0))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    0))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))))))))))), ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))), 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))) :: ((Place (B, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    0))) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0,
    0)) :: ((Place (D, (Stdlib.Int.succ 0), (Stdlib.Int.succ 0),
    (Stdlib.Int.succ 0))) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0)) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))), (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))) :: ((Place (G, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), 0, (Stdlib.Int.succ (Stdlib.Int.succ
    0)))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))))))))))), ((Place (A, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place
    (B, 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (C, (Stdlib.Int.succ
    0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    0)) :: ((Place (D, 0, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (E, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), 0)) :: ((Place (F, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0,
    (Stdlib.Int.succ 0))) :: ((Place (G, (Stdlib.Int.succ 0), 0,
    0)) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ 0))) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))))))),
    ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place
    (B, 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ 0))) :: ((Place (D, 0, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (E,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0,
    0)) :: ((Place (G, (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0,
    (Stdlib.Int.succ 0))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))))))))))))),
    ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))), 0)) :: ((Place (B, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ 0))) :: ((Place
    (C, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    0)) :: ((Place (D, 0, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (E, (Stdlib.Int.succ
    0), (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ
    0)))) :: ((Place (F, (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (G, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))) :: ((Place (H, 0, 0,
    0)) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))))))))))))))), ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ 0))) :: ((Place (B, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (C,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), 0)) :: ((Place (D, 0, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (E,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), 0, (Stdlib.Int.succ 0))) :: ((Place (G,
    (Stdlib.Int.succ 0), 0, 0)) :: ((Place (H, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    0)))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))))))))))))))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: ((Place (B, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0)) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))), 0,
    0)) :: ((Place (D, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (G, (Stdlib.Int.succ
    0), 0, (Stdlib.Int.succ 0))) :: ((Place (H, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))))))))))), ((Place (A, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place
    (B, (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place (C,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ 0))) :: ((Place (D, 0, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0)) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0, (Stdlib.Int.succ
    0))) :: ((Place (G, (Stdlib.Int.succ 0), 0, 0)) :: ((Place (H,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ
    0)))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))))))))))))))))), ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place
    (B, (Stdlib.Int.succ 0), 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))) :: ((Place (D, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ 0), 0)) :: ((Place (E, 0, 0,
    0)) :: ((Place (F, 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))), (Stdlib.Int.succ
    0))) :: ((Place (G, (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0)) :: ((Place
    (H, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))),
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ
    0)))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))))))))))))))))))), ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place
    (B, 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ 0))) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ 0))) :: ((Place
    (D, 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0)) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0,
    0)) :: ((Place (G, (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0,
    (Stdlib.Int.succ 0))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))), 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))))))))))))))))))), ((Place (A,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), 0)) :: ((Place (B, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))), 0,
    0)) :: ((Place (D, 0, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (E, (Stdlib.Int.succ
    0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (F, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0)) :: ((Place (G, (Stdlib.Int.succ 0), 0, (Stdlib.Int.succ
    0))) :: ((Place (H, (Stdlib.Int.succ 0), (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ
    0)))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))))))))))))))), ((Place (A,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))), (Stdlib.Int.succ
    0))) :: ((Place (B, (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place
    (C, (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place (D, 0, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (E, 0, 0, 0)) :: ((Place (F, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    0))) :: ((Place (G, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ 0), (Stdlib.Int.succ
    0))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))))))))))))))))))))), ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0)) :: ((Place (B, 0, 0,
    0)) :: ((Place (C, 0, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (D,
    0, (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    0))) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0)) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (G, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (H,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))))))))))))))))))))))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: ((Place (B, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), 0)) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place (D,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place
    (E, 0, 0, 0)) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))), (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))) :: ((Place (G, (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (H, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), (Stdlib.Int.succ 0), (Stdlib.Int.succ
    0))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))))))))))))))))))))))), ((Place (A,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (B, (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place
    (C, (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0, 0)) :: ((Place (D, 0, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0)) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0, (Stdlib.Int.succ
    0))) :: ((Place (G, 0, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ 0))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    0))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))))))))))))))))))), ((Place
    (A, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: ((Place (B, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))) :: ((Place (D, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ 0), 0)) :: ((Place (E, 0, 0,
    0)) :: ((Place (F, 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))), (Stdlib.Int.succ
    0))) :: ((Place (G, (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0)) :: ((Place
    (H, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))))))))))))))))))))))))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: ((Place (B, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), 0, 0)) :: ((Place (D, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), 0)) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (F, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (G,
    (Stdlib.Int.succ 0), 0, (Stdlib.Int.succ 0))) :: ((Place (H,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), 0)) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))))))))))))))))))))))))))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: ((Place (B, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))), 0,
    0)) :: ((Place (D, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    0)) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))), (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (G, (Stdlib.Int.succ 0), 0,
    (Stdlib.Int.succ 0))) :: ((Place (H, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))),
    (Stdlib.Int.succ 0))) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))))))))))))))))))))))))))),
    ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place (B, 0, 0,
    0)) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))),
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place
    (D, 0, 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (F, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), (Stdlib.Int.succ 0))) :: ((Place (G, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0)) :: ((Place (H,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))),
    (Stdlib.Int.succ 0)), ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))), 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (B, (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place (C, 0, 0,
    0)) :: ((Place (D, 0, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ 0))) :: ((Place (E, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ 0), (Stdlib.Int.succ 0))) :: ((Place (F,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (G, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))), 0)) :: ((Place (H,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    0)) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))),
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))) :: ((Place (B, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place
    (C, 0, 0, 0)) :: ((Place (D, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ 0))) :: ((Place (E, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ 0), (Stdlib.Int.succ 0))) :: ((Place (F,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (G, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    0)) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0)) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))), ((Place (A,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (B, (Stdlib.Int.succ
    0), (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))) :: ((Place (C, (Stdlib.Int.succ 0), 0, 0)) :: ((Place (D,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ 0))) :: ((Place
    (E, 0, (Stdlib.Int.succ 0), 0)) :: ((Place (F, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (G, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    0))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0)) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (B,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))) :: ((Place (C, (Stdlib.Int.succ 0), 0,
    0)) :: ((Place (D, (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ 0))) :: ((Place (E, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0)) :: ((Place (F,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (G, 0, (Stdlib.Int.succ 0),
    0)) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    0)) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))), ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place
    (B, (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), 0)) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (D,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (E, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0)) :: ((Place (F, 0, 0,
    0)) :: ((Place (G, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ 0), 0)) :: ((Place (H, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))) :: ((Place (B, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (C,
    (Stdlib.Int.succ 0), 0, 0)) :: ((Place (D, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), 0)) :: ((Place (E, 0, (Stdlib.Int.succ 0),
    0)) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place (G, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ 0))) :: ((Place
    (H, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), 0)) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))), ((Place (A,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (B,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), 0)) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (D,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (E, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0)) :: ((Place (F, 0, 0,
    0)) :: ((Place (G, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ 0), 0)) :: ((Place (H, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))) :: ((Place (B, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place
    (C, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (D, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), 0, (Stdlib.Int.succ 0))) :: ((Place (E,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0)) :: ((Place (F,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (G, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    0))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), 0, 0)) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))))))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))) :: ((Place (B, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place
    (C, 0, 0, 0)) :: ((Place (D, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ 0))) :: ((Place (E, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ 0), (Stdlib.Int.succ 0))) :: ((Place (F,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (G, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), 0)) :: ((Place (H, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    0)) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))))))), ((Place (A,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (B, (Stdlib.Int.succ
    0), (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))) :: ((Place (C, (Stdlib.Int.succ 0), 0, 0)) :: ((Place (D,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ 0))) :: ((Place
    (E, 0, (Stdlib.Int.succ 0), 0)) :: ((Place (F, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (G, 0, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ 0))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), 0)) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))))))))), ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))), 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (B, (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place (C,
    (Stdlib.Int.succ 0), 0, 0)) :: ((Place (D, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place (E,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0)) :: ((Place (F,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (G, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    0)))) :: ((Place (H, 0, (Stdlib.Int.succ 0),
    0)) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))))))))), ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place
    (B, 0, 0, (Stdlib.Int.succ 0))) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (D,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    0), (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (E,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0)) :: ((Place
    (F, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0,
    0)) :: ((Place (G, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (H,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    0)) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))))))))))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))) :: ((Place (B, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (C,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place (D,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0,
    (Stdlib.Int.succ 0))) :: ((Place (E, 0, 0, 0)) :: ((Place (F,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place (G,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ 0))) :: ((Place
    (H, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    0))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))))))))))), ((Place (A,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (B, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), 0)) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place
    (D, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0,
    (Stdlib.Int.succ 0))) :: ((Place (E, 0, 0, 0)) :: ((Place (F,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (G, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place
    (H, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    0))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))))))),
    ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (B,
    0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), 0)) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), 0, 0)) :: ((Place (D, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place
    (G, (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ 0))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))), 0, (Stdlib.Int.succ 0))) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))))))))))))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))) :: ((Place (B, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place
    (C, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    0)) :: ((Place (D, (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ 0))) :: ((Place (E, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0)) :: ((Place (F,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (G, (Stdlib.Int.succ (Stdlib.Int.succ
    0)), 0, (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (H,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0,
    0)) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))))))))))))))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))) :: ((Place (B, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place
    (C, (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), 0)) :: ((Place (D, (Stdlib.Int.succ 0), 0,
    0)) :: ((Place (E, (Stdlib.Int.succ 0), 0, (Stdlib.Int.succ
    0))) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (G, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    0))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), 0)) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))))))))))))))),
    ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (B,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))) :: ((Place (C, (Stdlib.Int.succ 0), 0,
    0)) :: ((Place (D, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    0)) :: ((Place (E, 0, (Stdlib.Int.succ 0), 0)) :: ((Place (F,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (G, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ 0))) :: ((Place (H,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), 0)) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))))))))))),
    ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (B,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (D, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), 0, (Stdlib.Int.succ 0))) :: ((Place (E, 0, 0, 0)) :: ((Place (F,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place (G,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ 0))) :: ((Place
    (H, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    0))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))))))))))))))))), ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place
    (B, (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0)) :: ((Place (C,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (D, 0, (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (E, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (F,
    (Stdlib.Int.succ 0), 0, 0)) :: ((Place (G, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ 0), 0)) :: ((Place (H, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    0))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))))))))))))))))))), ((Place (A, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (B,
    0, (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (C,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ 0))) :: ((Place (D, 0, 0,
    (Stdlib.Int.succ 0))) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0)) :: ((Place (F, (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place (G,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (H,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), 0, 0)) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))))))))))))))))))), ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))), 0)) :: ((Place
    (B, 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ 0), 0)) :: ((Place (D,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (E,
    (Stdlib.Int.succ 0), 0, 0)) :: ((Place (F, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ 0))) :: ((Place (G, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))), (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))))))))))))))))))))), ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), (Stdlib.Int.succ 0))) :: ((Place (B, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place
    (D, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (E,
    0, (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (F,
    0, 0, 0)) :: ((Place (G, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ 0), 0)) :: ((Place (H,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    0))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))))))))))))))))))))), ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0)) :: ((Place (B, 0, 0,
    0)) :: ((Place (C, 0, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (D,
    0, (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    0))) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0)) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (G,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (H,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))))))))))))))))))))))), ((Place (A,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (B, (Stdlib.Int.succ
    0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0)) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0,
    0)) :: ((Place (D, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (E, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (G, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ 0))) :: ((Place
    (H, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))), 0, (Stdlib.Int.succ
    0))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))))))))))))))))))))))), ((Place
    (A, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (B, 0, 0,
    0)) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place
    (D, 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ 0))) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (F,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: ((Place (G, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0)) :: ((Place (H,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))),
    0)) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))))))))))))))))))))))))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), 0, 0)) :: ((Place (B, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), 0)) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))) :: ((Place (D, 0, 0, (Stdlib.Int.succ
    0))) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0)) :: ((Place (F,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    0)))) :: ((Place (G, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (H, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))))))))))))))))))))))))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))) :: ((Place (B, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place
    (C, (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place (D,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0,
    (Stdlib.Int.succ 0))) :: ((Place (E, 0, 0, 0)) :: ((Place (F,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (G,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ 0))) :: ((Place (H,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))))))))))))))))))))),
    ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (B,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    0)) :: ((Place (D, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ 0))) :: ((Place (E, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ 0))) :: ((Place
    (F, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (G, (Stdlib.Int.succ (Stdlib.Int.succ
    0)), 0, (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (H,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0,
    0)) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))))))))))))))))))))))))))),
    ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place (B, 0, 0,
    0)) :: ((Place (C, 0, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    0)) :: ((Place (D, 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (E, (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ 0))) :: ((Place (G, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))))))))))))))))))))))))))))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))) :: ((Place (B, 0, 0, 0)) :: ((Place (C,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place (D,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0)) :: ((Place (E,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0,
    (Stdlib.Int.succ 0))) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0)) :: ((Place (G,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place
    (H, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))), (Stdlib.Int.succ 0)), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), 0, 0)) :: ((Place (B, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (C,
    (Stdlib.Int.succ 0), 0, (Stdlib.Int.succ 0))) :: ((Place (D,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), 0)) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place
    (F, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))) :: ((Place (G, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (H,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))),
    0)) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))), (Stdlib.Int.succ (Stdlib.Int.succ 0))), ((Place (A,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), 0)) :: ((Place (B, 0, 0, 0)) :: ((Place (C,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (D, (Stdlib.Int.succ (Stdlib.Int.succ
    0)), 0, (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (E,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))) :: ((Place (F, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ 0))) :: ((Place
    (G, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), 0)) :: ((Place (H, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), ((Place (A, 0, 0, 0)) :: ((Place (B, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), 0)) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))) :: ((Place (D, 0, (Stdlib.Int.succ 0),
    0)) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0)) :: ((Place (F,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (G, (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (H,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))), 0)) :: ((Place
    (B, 0, (Stdlib.Int.succ 0), 0)) :: ((Place (C, (Stdlib.Int.succ 0), 0,
    0)) :: ((Place (D, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ 0))) :: ((Place (E, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), 0)) :: ((Place (F, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (G, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (H,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0,
    (Stdlib.Int.succ 0))) :: ((Place (B, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))), 0)) :: ((Place
    (C, (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0, 0)) :: ((Place (D,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    0)), 0)) :: ((Place (E, 0, (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (F, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (G,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    0)))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))), ((Place (A, 0, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    0)))) :: ((Place (B, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (C,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (D, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0)) :: ((Place (E,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0)) :: ((Place (F,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))),
    (Stdlib.Int.succ 0), (Stdlib.Int.succ 0))) :: ((Place (G,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ 0))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ
    0)), 0, 0)) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))))), ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place
    (B, 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))), 0,
    0)) :: ((Place (D, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0)) :: ((Place (F, 0, 0, (Stdlib.Int.succ 0))) :: ((Place (G,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    0), (Stdlib.Int.succ 0))) :: ((Place (H, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    0))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    0)) :: ((Place (B, (Stdlib.Int.succ 0), 0, 0)) :: ((Place (C,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place (D,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (E, 0, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (F, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (G, (Stdlib.Int.succ
    0), (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (H, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), (Stdlib.Int.succ 0))) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))), ((Place
    (A, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), 0)) :: ((Place (B, (Stdlib.Int.succ 0), 0,
    0)) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place
    (D, (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place
    (E, 0, 0, (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (F,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (G,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (H, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    0)) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))))))), ((Place
    (A, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place (B, 0, 0,
    0)) :: ((Place (C, 0, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (D,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ 0))) :: ((Place
    (E, 0, (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0)) :: ((Place (F,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (G, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ
    0)), 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))))))))), ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))), 0)) :: ((Place
    (B, (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), 0)) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))) :: ((Place (D, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: ((Place (E, (Stdlib.Int.succ 0), 0,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (F, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (G,
    (Stdlib.Int.succ 0), 0, 0)) :: ((Place (H, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ 0), (Stdlib.Int.succ
    0))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))))))))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), (Stdlib.Int.succ 0), 0)) :: ((Place (B, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))) :: ((Place (C, 0, 0, 0)) :: ((Place (D, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: ((Place (E, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ 0))) :: ((Place
    (F, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))),
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (G, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), 0)) :: ((Place (H, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ
    0)))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))))), ((Place (A,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))) :: ((Place (B, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ 0), (Stdlib.Int.succ
    0))) :: ((Place (D, 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), 0)) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0)) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (G, (Stdlib.Int.succ
    0), 0, 0)) :: ((Place (H, (Stdlib.Int.succ 0), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))))))))))), ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (B, (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0)) :: ((Place
    (C, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), 0, 0)) :: ((Place (D, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: ((Place (E, 0, 0, (Stdlib.Int.succ
    0))) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))), 0, (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (G,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (H, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))))))))))))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), 0)) :: ((Place (B, (Stdlib.Int.succ 0), 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (C,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ 0))) :: ((Place
    (D, (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (F,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0, 0)) :: ((Place (G,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0, (Stdlib.Int.succ
    0))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))), (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))))))))))))), ((Place (A,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), 0)) :: ((Place (B, (Stdlib.Int.succ 0), 0,
    0)) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place
    (D, (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place
    (E, 0, 0, (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (F,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (G, (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place (H,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))))))))), ((Place (A,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ 0))) :: ((Place
    (B, (Stdlib.Int.succ 0), 0, 0)) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    0)) :: ((Place (D, (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (F,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: ((Place (G, (Stdlib.Int.succ (Stdlib.Int.succ
    0)), 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))), (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))))))))))))))), ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (B, (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place
    (C, 0, 0, (Stdlib.Int.succ 0))) :: ((Place (D, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ
    0)), 0, 0)) :: ((Place (F, 0, (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: ((Place (G, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place (H,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0)) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))))))))))))))))), ((Place (A, 0, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: ((Place (B, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (D,
    0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    0)) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0)) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))),
    (Stdlib.Int.succ 0), (Stdlib.Int.succ 0))) :: ((Place (G,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), 0, 0)) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))))))))))))))))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), 0)) :: ((Place (B, (Stdlib.Int.succ 0), 0,
    0)) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))) :: ((Place (D, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ 0))) :: ((Place (E, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), 0)) :: ((Place (F, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (G, (Stdlib.Int.succ
    0), (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (H, 0, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ
    0)))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))))))))))))), ((Place (A, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (B, 0, 0, 0)) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))), 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (D, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0)) :: ((Place (E,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0,
    (Stdlib.Int.succ 0))) :: ((Place (F, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))),
    (Stdlib.Int.succ 0))) :: ((Place (G, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0)) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))),
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))))))))))))))))))), ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))), 0)) :: ((Place
    (B, (Stdlib.Int.succ 0), 0, 0)) :: ((Place (C, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ 0))) :: ((Place (D, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    0)) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place (G, 0, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (H, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))))))))))))))))))))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), (Stdlib.Int.succ 0))) :: ((Place (B, 0, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), 0)) :: ((Place (D, (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (E, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (F,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))),
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (G, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ 0))) :: ((Place
    (H, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0,
    0)) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))))))))))))))))))))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    0)) :: ((Place (B, 0, 0, 0)) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (D, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ 0))) :: ((Place (E,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0)) :: ((Place (F,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (G, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))))))))))))))))), ((Place
    (A, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place (B, 0, 0,
    0)) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))), 0,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (D, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ 0))) :: ((Place (E, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0)) :: ((Place (F,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (G,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))))))))))))))))))))))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), 0)) :: ((Place (B, (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place
    (C, 0, 0, (Stdlib.Int.succ 0))) :: ((Place (D, 0, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (E, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), 0, 0)) :: ((Place (F, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    0))) :: ((Place (G, (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place (H,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))))))))))))))))))))))))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), (Stdlib.Int.succ 0))) :: ((Place (B, 0, 0,
    0)) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))) :: ((Place (D, (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), 0)) :: ((Place (E, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0, (Stdlib.Int.succ
    0))) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (G, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (H,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    0)) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))))))))))))))))))))))))), ((Place (A,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), 0)) :: ((Place (B, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place
    (C, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (D, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ 0))) :: ((Place (E, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), 0)) :: ((Place (F, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ 0), 0)) :: ((Place (G, 0, 0, 0)) :: ((Place (H,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))))))))))))))))))))),
    ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place
    (B, 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (D, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), 0, (Stdlib.Int.succ (Stdlib.Int.succ
    0)))) :: ((Place (E, (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (F, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), (Stdlib.Int.succ 0), 0)) :: ((Place (G, 0, 0, 0)) :: ((Place (H,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))))))))))))))))))))))))))), ((Place (A, 0, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (B,
    (Stdlib.Int.succ 0), 0, 0)) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), 0)) :: ((Place (D, 0, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (E,
    0, 0, (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (F, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), (Stdlib.Int.succ 0))) :: ((Place (G,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place (H,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    0))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ 0)), ((Place (A,
    (Stdlib.Int.succ 0), 0, 0)) :: ((Place (B, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (C, (Stdlib.Int.succ 0), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: ((Place (D, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))) :: ((Place (E, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), 0)) :: ((Place (G, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ 0))) :: ((Place (H, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    0))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    ((Place (A, (Stdlib.Int.succ 0), 0, 0)) :: ((Place (B, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ 0))) :: ((Place (C, (Stdlib.Int.succ 0), 0,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (D, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (E, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), 0)) :: ((Place (F, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (G, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0)) :: ((Place (H,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), ((Place (A, (Stdlib.Int.succ 0),
    0, 0)) :: ((Place (B, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (C, (Stdlib.Int.succ 0), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: ((Place (D, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ 0))) :: ((Place
    (E, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (F,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (G, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0)) :: ((Place (H,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))),
    0)) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))), ((Place (A, (Stdlib.Int.succ
    0), 0, 0)) :: ((Place (B, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (C,
    (Stdlib.Int.succ 0), 0, (Stdlib.Int.succ (Stdlib.Int.succ
    0)))) :: ((Place (D, (Stdlib.Int.succ 0), (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (E, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    0))) :: ((Place (G, (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0)) :: ((Place
    (H, 0, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    0)) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))), ((Place (A,
    0, 0, 0)) :: ((Place (B, (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place (C,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (D, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place (E,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0)) :: ((Place (F,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ 0), 0)) :: ((Place (G, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    0)))) :: ((Place (H, (Stdlib.Int.succ 0), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))), ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (B,
    0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), 0)) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0, 0)) :: ((Place (D, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0)) :: ((Place (F,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))) :: ((Place (G, (Stdlib.Int.succ 0), 0,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (H, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))), 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))), ((Place (A, 0, 0,
    0)) :: ((Place (B, 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))), 0)) :: ((Place (C,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (D,
    0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (E, (Stdlib.Int.succ
    0), 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ 0), 0)) :: ((Place (G,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place (H,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), 0)) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))))), ((Place
    (A, (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))) :: ((Place (B, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    0))) :: ((Place (C, (Stdlib.Int.succ 0), 0, 0)) :: ((Place (D,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (E, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), 0)) :: ((Place (F, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (G, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))),
    (Stdlib.Int.succ 0), 0)) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))))))), ((Place (A, (Stdlib.Int.succ 0), 0, 0)) :: ((Place (B, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ 0))) :: ((Place (C, (Stdlib.Int.succ 0), 0,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (D, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (E, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), 0)) :: ((Place (F, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (G, 0, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    0)) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))))))), ((Place (A, (Stdlib.Int.succ 0), 0, 0)) :: ((Place (B, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), 0)) :: ((Place (C, (Stdlib.Int.succ 0), 0,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (D, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (E, 0, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    0)) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (G, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ 0))) :: ((Place (H,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))))))))), ((Place (A, 0, 0, 0)) :: ((Place (B, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), 0)) :: ((Place (C, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0)) :: ((Place (D, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: ((Place (E, (Stdlib.Int.succ 0), 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (F,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place (G,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))), (Stdlib.Int.succ 0), 0)) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))))))))),
    ((Place (A, 0, 0, 0)) :: ((Place (B, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place
    (C, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place (D, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), 0)) :: ((Place (F, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ 0), 0)) :: ((Place (G, (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (H,
    (Stdlib.Int.succ 0), 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))))))))))), ((Place (A, 0, 0, 0)) :: ((Place (B, (Stdlib.Int.succ
    0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    0)) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place
    (D, 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (E, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0)) :: ((Place (F,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ 0), 0)) :: ((Place (G, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (H, (Stdlib.Int.succ
    0), 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))))))))))), ((Place (A, (Stdlib.Int.succ 0), 0, 0)) :: ((Place (B,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), 0)) :: ((Place (C, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (D,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (E, (Stdlib.Int.succ 0), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place
    (G, (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place
    (H, (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), 0)) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))))))), ((Place (A, 0, 0,
    0)) :: ((Place (B, (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place
    (D, 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (E, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0)) :: ((Place (F,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ 0), 0)) :: ((Place (G, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), 0)) :: ((Place (H, (Stdlib.Int.succ 0), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))))))))))))), ((Place (A, 0, 0,
    0)) :: ((Place (B, (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place
    (D, 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (E, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0)) :: ((Place (F,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ 0), 0)) :: ((Place (G, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place
    (H, (Stdlib.Int.succ 0), 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))))))))))))))), ((Place (A, 0, 0, 0)) :: ((Place (B, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))) :: ((Place (D, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (E, (Stdlib.Int.succ
    0), 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (F, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))),
    (Stdlib.Int.succ 0))) :: ((Place (G, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0)) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ 0),
    0)) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))))))))))))))), ((Place (A, (Stdlib.Int.succ 0), 0, 0)) :: ((Place
    (B, (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), 0)) :: ((Place (C, (Stdlib.Int.succ 0), 0,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (D, (Stdlib.Int.succ
    0), (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (E, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0)) :: ((Place (F,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    0)) :: ((Place (G, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (H, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))))))))))), ((Place (A,
    (Stdlib.Int.succ 0), 0, 0)) :: ((Place (B, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place
    (C, (Stdlib.Int.succ 0), 0, (Stdlib.Int.succ (Stdlib.Int.succ
    0)))) :: ((Place (D, (Stdlib.Int.succ 0), (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0)) :: ((Place (F, (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), 0)) :: ((Place (G, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (H, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))))))))))))))))), ((Place (A, 0, 0, 0)) :: ((Place (B, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), 0)) :: ((Place (C, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0)) :: ((Place (D, (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place
    (E, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (F,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))) :: ((Place (G, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ 0),
    0)) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))))))))))))))))))), ((Place (A, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (B, (Stdlib.Int.succ
    0), (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0)) :: ((Place (C,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (D, 0, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: ((Place (F, 0, 0, 0)) :: ((Place (G, 0,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place
    (H, (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), 0)) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))))))))))))))))))), ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))), 0)) :: ((Place
    (B, (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place (C,
    (Stdlib.Int.succ 0), 0, 0)) :: ((Place (D, 0, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (E,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (F, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0)) :: ((Place (G, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ 0),
    0)) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))))))))))))))))))))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), (Stdlib.Int.succ 0))) :: ((Place (B, 0, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0)) :: ((Place (D, 0, 0,
    0)) :: ((Place (E, 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), 0)) :: ((Place (F, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))) :: ((Place (G, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))))))))))))))))))))), ((Place (A,
    (Stdlib.Int.succ 0), 0, 0)) :: ((Place (B, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), 0)) :: ((Place (C, (Stdlib.Int.succ 0), 0,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (D, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (E, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), 0)) :: ((Place (F, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (G, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ 0))) :: ((Place
    (H, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))))))))))))))))))))))), ((Place (A, 0, 0, 0)) :: ((Place (B, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (D,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    0), (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (E,
    (Stdlib.Int.succ 0), 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (F, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0)) :: ((Place
    (G, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), 0)) :: ((Place (H, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ 0),
    0)) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))))))))))))))))))))))), ((Place (A, (Stdlib.Int.succ 0), 0,
    0)) :: ((Place (B, 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))), 0)) :: ((Place (C,
    (Stdlib.Int.succ 0), 0, (Stdlib.Int.succ (Stdlib.Int.succ
    0)))) :: ((Place (D, (Stdlib.Int.succ 0), (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (E, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (F, 0, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    0)) :: ((Place (G, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    0)) :: ((Place (H, (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))))))))))))))))))))))))), ((Place (A,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))), (Stdlib.Int.succ
    0))) :: ((Place (B, 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ 0))) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))) :: ((Place (D, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (E, 0, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (F, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), 0)) :: ((Place (G, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), 0, 0)) :: ((Place (H, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))))))))))))))))))))))))), ((Place
    (A, (Stdlib.Int.succ 0), 0, 0)) :: ((Place (B, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), 0)) :: ((Place (C, (Stdlib.Int.succ 0), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: ((Place (D, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (F,
    0, (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0)) :: ((Place (G,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place (H,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))))))))))))))))))))))))))), ((Place (A, (Stdlib.Int.succ 0), 0,
    0)) :: ((Place (B, 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (C, (Stdlib.Int.succ
    0), 0, (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (D,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (E,
    0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (F,
    0, (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0)) :: ((Place (G,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place (H,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))),
    0)) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))))))))))))))))))))))))))), ((Place (A, (Stdlib.Int.succ 0), 0,
    0)) :: ((Place (B, 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (C, (Stdlib.Int.succ
    0), 0, (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (D,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (E,
    0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (F,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    0)) :: ((Place (G, 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))), 0)) :: ((Place (H,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), 0)) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))))))))))))))))))))))),
    ((Place (A, 0, 0, 0)) :: ((Place (B, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place
    (D, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place
    (E, (Stdlib.Int.succ 0), 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ 0))) :: ((Place (G, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0)) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ 0),
    0)) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))), (Stdlib.Int.succ 0)),
    ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))), 0, (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (B, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ 0))) :: ((Place (C, 0, 0, 0)) :: ((Place (D,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place
    (E, (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0)) :: ((Place
    (F, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place (G,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    0)) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ 0), 0)) :: ((Place (B,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))) :: ((Place (C, (Stdlib.Int.succ 0), 0,
    0)) :: ((Place (D, 0, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (E, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), 0)) :: ((Place (F, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ 0))) :: ((Place (G, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (H, (Stdlib.Int.succ
    0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ (Stdlib.Int.succ
    0)))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0, 0)) :: ((Place (B, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), 0)) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))) :: ((Place (D, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (E, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (F, 0, 0, (Stdlib.Int.succ 0))) :: ((Place (G,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place (H,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), ((Place (A, 0, 0, 0)) :: ((Place (B, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ 0))) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), 0)) :: ((Place (D, (Stdlib.Int.succ 0), (Stdlib.Int.succ 0),
    0)) :: ((Place (E, (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (G, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (H, (Stdlib.Int.succ
    0), 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))), ((Place (A, 0, 0, 0)) :: ((Place (B, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place
    (C, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place
    (D, (Stdlib.Int.succ 0), (Stdlib.Int.succ 0), 0)) :: ((Place (E,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0)) :: ((Place
    (F, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (G, (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (H, (Stdlib.Int.succ 0), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))), ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), (Stdlib.Int.succ 0))) :: ((Place (B, 0, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))) :: ((Place (D, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place
    (E, 0, 0, 0)) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), 0)) :: ((Place (G, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    0))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))), ((Place (A, 0, 0,
    0)) :: ((Place (B, 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))), 0)) :: ((Place (C,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (D, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ
    0)))) :: ((Place (E, (Stdlib.Int.succ 0), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (F, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ 0), 0)) :: ((Place (G,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ 0))) :: ((Place
    (H, 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))))), ((Place
    (A, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0,
    0)) :: ((Place (B, (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0)) :: ((Place (C,
    (Stdlib.Int.succ 0), 0, (Stdlib.Int.succ 0))) :: ((Place (D, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place (G,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ (Stdlib.Int.succ
    0)))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))))))), ((Place (A, 0, 0, 0)) :: ((Place (B, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ 0),
    0)) :: ((Place (D, 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (E, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (F, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), 0)) :: ((Place (G, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ 0))) :: ((Place (H,
    (Stdlib.Int.succ 0), 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))))))), ((Place (A,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0,
    0)) :: ((Place (B, 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))), 0)) :: ((Place (C,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    0)) :: ((Place (D, 0, 0, (Stdlib.Int.succ 0))) :: ((Place (E,
    (Stdlib.Int.succ 0), 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (G,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (H,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))), ((Place (A, 0, 0,
    0)) :: ((Place (B, 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))), 0)) :: ((Place (C,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), 0)) :: ((Place (D, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (E, (Stdlib.Int.succ
    0), 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place
    (G, (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))),
    (Stdlib.Int.succ 0), 0)) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))))))))), ((Place (A, 0, 0, 0)) :: ((Place (B, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), 0)) :: ((Place (C, 0, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (D,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ 0), 0)) :: ((Place (G, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ 0))) :: ((Place (H, (Stdlib.Int.succ 0), 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))))))))))), ((Place (A, 0, 0, 0)) :: ((Place (B, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: ((Place (D, (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    0)) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0)) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ 0), 0)) :: ((Place (G, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))) :: ((Place (H, (Stdlib.Int.succ 0), 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))))))))))), ((Place (A, 0, 0, 0)) :: ((Place (B,
    0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), 0)) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), (Stdlib.Int.succ 0), 0)) :: ((Place (D, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (E,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place (G,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (H, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    0))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))))))), ((Place (A, 0, 0,
    0)) :: ((Place (B, (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))) :: ((Place (C, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ 0))) :: ((Place
    (D, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    0))) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ 0), 0)) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (G,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), 0)) :: ((Place (H, (Stdlib.Int.succ 0), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))))))))))))),
    ((Place (A, 0, 0, 0)) :: ((Place (B, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))), 0)) :: ((Place
    (C, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place (D, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ 0))) :: ((Place
    (E, (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (F,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    0), 0)) :: ((Place (G, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (H, (Stdlib.Int.succ
    0), 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))))))))))))))), ((Place (A, 0, 0, 0)) :: ((Place (B, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))) :: ((Place (D, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (E, (Stdlib.Int.succ
    0), 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (F, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))),
    (Stdlib.Int.succ 0))) :: ((Place (G, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0)) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))),
    (Stdlib.Int.succ 0), 0)) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))))))))))))))), ((Place (A, 0, 0,
    0)) :: ((Place (B, (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), 0)) :: ((Place (C, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (D, (Stdlib.Int.succ 0), (Stdlib.Int.succ 0),
    0)) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0)) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (G, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (H,
    (Stdlib.Int.succ 0), 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))))))))))),
    ((Place (A, 0, 0, 0)) :: ((Place (B, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ 0))) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), 0)) :: ((Place (D, (Stdlib.Int.succ 0), (Stdlib.Int.succ 0),
    0)) :: ((Place (E, (Stdlib.Int.succ 0), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (F, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))) :: ((Place (G, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))))))))))))))))), ((Place (A, (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (B,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), 0)) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), 0, 0)) :: ((Place (D, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (E, 0, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (G,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0, (Stdlib.Int.succ
    0))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))))))))))))))))))), ((Place (A, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (B,
    0, 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ 0))) :: ((Place
    (D, (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    0)) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0)) :: ((Place (F, (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ 0))) :: ((Place (G, 0, 0,
    0)) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))))))))))))))))))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), 0)) :: ((Place (B, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (C,
    0, 0, 0)) :: ((Place (D, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ 0), 0)) :: ((Place (E,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    0), (Stdlib.Int.succ 0))) :: ((Place (F, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0)) :: ((Place
    (G, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (H, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))))))))))))))), ((Place (A,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))), (Stdlib.Int.succ
    0))) :: ((Place (B, (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place
    (C, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (D,
    0, 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (E, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0)) :: ((Place (F,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: ((Place (G, 0, 0, 0)) :: ((Place (H,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    0)), 0)) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))))))))))))))))))))), ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0)) :: ((Place (B,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (C, 0, 0,
    0)) :: ((Place (D, 0, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (E, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), 0)) :: ((Place (F, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), (Stdlib.Int.succ 0), (Stdlib.Int.succ 0))) :: ((Place (G,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place (H, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))))))))))))))))))))))), ((Place (A, 0, 0, 0)) :: ((Place (B, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), 0)) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))) :: ((Place (D, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (E, (Stdlib.Int.succ
    0), 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ 0), 0)) :: ((Place (G, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ 0))) :: ((Place (H, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))))))))))))))))))))))), ((Place
    (A, 0, 0, 0)) :: ((Place (B, (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ 0))) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    0)) :: ((Place (D, (Stdlib.Int.succ 0), (Stdlib.Int.succ 0),
    0)) :: ((Place (E, (Stdlib.Int.succ 0), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (F, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))) :: ((Place (G, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (H, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))))))))))))))))))), ((Place
    (A, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))), (Stdlib.Int.succ
    0))) :: ((Place (B, 0, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))) :: ((Place (D, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place
    (E, 0, 0, 0)) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), 0)) :: ((Place (G, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    0))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))))))))))))))))))))))))), ((Place (A, 0, 0, 0)) :: ((Place (B,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ 0))) :: ((Place (C,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), 0)) :: ((Place (D, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ 0), 0)) :: ((Place (E, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (F, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place
    (G, (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (H, (Stdlib.Int.succ
    0), 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))))))))))))))))))))))))))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0, 0)) :: ((Place (B,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place
    (D, 0, 0, (Stdlib.Int.succ 0))) :: ((Place (E, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (F, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (G, 0, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (H, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), 0)) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))))))))))))))))))))))))))),
    ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place (B, 0, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (C,
    0, (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0)) :: ((Place (D, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (E,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    0))) :: ((Place (G, 0, 0, 0)) :: ((Place (H, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))))))))))))))))))))))),
    ((Place (A, 0, 0, 0)) :: ((Place (B, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place
    (D, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place
    (E, (Stdlib.Int.succ 0), 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ 0))) :: ((Place (G, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0)) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))),
    (Stdlib.Int.succ 0), 0)) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))))), (Stdlib.Int.succ 0)), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), 0, 0)) :: ((Place (B, (Stdlib.Int.succ 0), 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place (D,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (E, (Stdlib.Int.succ
    0), 0, (Stdlib.Int.succ 0))) :: ((Place (F, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))) :: ((Place (G, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ 0))) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))))), (Stdlib.Int.succ (Stdlib.Int.succ 0))), ((Place (A,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))) :: ((Place (B, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), 0)) :: ((Place (C, (Stdlib.Int.succ 0), 0, 0)) :: ((Place (D,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (E, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    0)), 0)) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (G, (Stdlib.Int.succ
    0), (Stdlib.Int.succ 0), 0)) :: ((Place (H, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0,
    0)) :: ((Place (B, 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))), 0)) :: ((Place (C, 0, 0,
    (Stdlib.Int.succ 0))) :: ((Place (D, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (E, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (F, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (G,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place (H, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), ((Place (A, (Stdlib.Int.succ 0), (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (B,
    (Stdlib.Int.succ 0), 0, 0)) :: ((Place (C, (Stdlib.Int.succ 0), 0,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (D, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place
    (E, (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0)) :: ((Place
    (F, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (G, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    0)))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))), ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))), (Stdlib.Int.succ 0), 0)) :: ((Place (B, 0, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))) :: ((Place (D, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place
    (E, 0, 0, 0)) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (G,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    0)) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (B, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (D,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    0)) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0)) :: ((Place (F, 0, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    0)))) :: ((Place (G, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ 0))) :: ((Place (H, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0,
    0)) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))), ((Place (A,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0, 0)) :: ((Place (B, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), 0)) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place (D,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    0), (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (E,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    0)), 0)) :: ((Place (F, (Stdlib.Int.succ 0), 0, (Stdlib.Int.succ
    0))) :: ((Place (G, (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (H, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0, 0)) :: ((Place (B,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))) :: ((Place (C, 0, 0, (Stdlib.Int.succ
    0))) :: ((Place (D, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0)) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (G,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place
    (H, 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    0))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))))))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (B, (Stdlib.Int.succ
    0), 0, 0)) :: ((Place (C, (Stdlib.Int.succ 0), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: ((Place (D, (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (E,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (F, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), 0)) :: ((Place (G, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ 0))) :: ((Place (H,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))))))), ((Place
    (A, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0,
    0)) :: ((Place (B, 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (D, 0, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    0)) :: ((Place (G, 0, 0, (Stdlib.Int.succ 0))) :: ((Place (H,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ 0))) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))))))))), ((Place (A, (Stdlib.Int.succ 0), (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (B,
    (Stdlib.Int.succ 0), 0, 0)) :: ((Place (C, (Stdlib.Int.succ 0), 0,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (D, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place
    (E, (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0)) :: ((Place
    (F, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (G, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    0)))) :: ((Place (H, (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))))))))), ((Place (A, (Stdlib.Int.succ 0), (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (B,
    (Stdlib.Int.succ 0), 0, 0)) :: ((Place (C, (Stdlib.Int.succ 0), 0,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (D, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    0)) :: ((Place (G, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ 0))) :: ((Place (H, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ 0))) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))))), ((Place (A,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))) :: ((Place (B, (Stdlib.Int.succ 0), 0,
    0)) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (D, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place (E,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0)) :: ((Place (F,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (G, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    0)))) :: ((Place (H, (Stdlib.Int.succ 0), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))))))))))),
    ((Place (A, (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (B, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))), 0)) :: ((Place
    (C, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (D, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ 0), (Stdlib.Int.succ
    0))) :: ((Place (E, (Stdlib.Int.succ 0), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place
    (G, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place
    (H, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0,
    0)) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))))))),
    ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place
    (B, (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))) :: ((Place (C, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), 0)) :: ((Place (D, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ 0))) :: ((Place (E,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    0), (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (F,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (G, 0, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (H, 0, 0,
    0)) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))))))))))))), ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))), 0)) :: ((Place
    (B, 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (C, (Stdlib.Int.succ
    0), 0, 0)) :: ((Place (D, (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (E,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (F, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))) :: ((Place (G, (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place (H,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ 0),
    0)) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))))))))))))))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ 0))) :: ((Place (B,
    (Stdlib.Int.succ 0), 0, 0)) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    0)) :: ((Place (D, (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (E,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (F,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))) :: ((Place (G, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (H, (Stdlib.Int.succ
    0), 0, (Stdlib.Int.succ (Stdlib.Int.succ
    0)))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))))))))))))))), ((Place (A,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0,
    0)) :: ((Place (B, (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))) :: ((Place (C, 0, 0, (Stdlib.Int.succ
    0))) :: ((Place (D, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0)) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (G,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place
    (H, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), 0)) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))))))))))))))))), ((Place (A, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ 0))) :: ((Place
    (B, (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place (C, 0, 0,
    0)) :: ((Place (D, 0, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (E, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place
    (F, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ 0), 0)) :: ((Place (G, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (H, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))),
    0)) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))))))))))))))))), ((Place (A, (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (B,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), 0)) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), 0, 0)) :: ((Place (D, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (E, 0, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (G,
    (Stdlib.Int.succ 0), 0, (Stdlib.Int.succ 0))) :: ((Place (H,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))))))))))))))))))), ((Place (A, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (B,
    (Stdlib.Int.succ 0), 0, 0)) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    0)) :: ((Place (D, 0, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (E, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (F,
    0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), (Stdlib.Int.succ 0))) :: ((Place (G,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: ((Place (H, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    0))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))))))))))))))))))), ((Place (A,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), 0)) :: ((Place (B, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (C, (Stdlib.Int.succ 0), 0, 0)) :: ((Place (D, 0, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))) :: ((Place (F, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0)) :: ((Place
    (G, (Stdlib.Int.succ 0), (Stdlib.Int.succ 0), 0)) :: ((Place (H,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ 0))) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))))))))))))))))))))), ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), (Stdlib.Int.succ 0))) :: ((Place (B, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place
    (D, 0, 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ
    0)), 0, 0)) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ 0))) :: ((Place
    (G, 0, 0, (Stdlib.Int.succ 0))) :: ((Place (H, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))))))))))))))))))))), ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0)) :: ((Place (B, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (C, 0, 0, 0)) :: ((Place (D, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (E,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0)) :: ((Place (F,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))) :: ((Place (G, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ 0),
    (Stdlib.Int.succ 0))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))))))))))))))))))))))), ((Place (A,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (B, (Stdlib.Int.succ
    0), 0, 0)) :: ((Place (C, (Stdlib.Int.succ 0), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: ((Place (D, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place (E,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0)) :: ((Place (F,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ (Stdlib.Int.succ
    0)))) :: ((Place (G, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    0)))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))))))))))))))))))))))), ((Place
    (A, 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (B, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0)) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))), 0,
    0)) :: ((Place (D, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (E, 0, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (G,
    (Stdlib.Int.succ 0), 0, (Stdlib.Int.succ 0))) :: ((Place (H,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))))))))))))))))))))))))), ((Place (A, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place (B, 0, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (C,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place (D,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ 0), (Stdlib.Int.succ
    0))) :: ((Place (E, 0, 0, 0)) :: ((Place (F, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), (Stdlib.Int.succ 0))) :: ((Place (G, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0)) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))))))))))))))))))))))))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ
    0)))) :: ((Place (B, (Stdlib.Int.succ 0), 0, 0)) :: ((Place (C,
    (Stdlib.Int.succ 0), 0, (Stdlib.Int.succ (Stdlib.Int.succ
    0)))) :: ((Place (D, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), 0)) :: ((Place (E, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), 0)) :: ((Place (F, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (G, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (H, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))))))))))))))))))))))))))), ((Place (A,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (B, (Stdlib.Int.succ
    0), 0, 0)) :: ((Place (C, (Stdlib.Int.succ 0), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: ((Place (D, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    0)) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    0))) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (G, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (H, (Stdlib.Int.succ
    0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), 0)) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))))))))))))))))))))))))))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    0)) :: ((Place (B, 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (C, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0)) :: ((Place (D,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (E,
    0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (F,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ 0))) :: ((Place
    (G, 0, 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (H, 0, 0, 0)) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))))))), (Stdlib.Int.succ 0)), ((Place (A,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0, 0)) :: ((Place (B, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), 0)) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (D, 0, 0, (Stdlib.Int.succ 0))) :: ((Place (E,
    (Stdlib.Int.succ 0), 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))), (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (G, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), 0)) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))))))), (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))), 0)) :: ((Place (B, 0, 0,
    0)) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), 0)) :: ((Place (D, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (E, (Stdlib.Int.succ
    0), 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place (G,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ 0))) :: ((Place
    (H, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), ((Place (A, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place
    (B, 0, 0, 0)) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0)) :: ((Place (D,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ 0))) :: ((Place (E, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place
    (G, (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (H, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))),
    0)) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))), 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))) :: ((Place (B, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), 0)) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), 0)) :: ((Place (D, 0, 0, 0)) :: ((Place (E,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ 0),
    0)) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place (G, 0, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (H, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))) :: ((Place (B, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ 0))) :: ((Place
    (C, (Stdlib.Int.succ 0), 0, (Stdlib.Int.succ 0))) :: ((Place (D,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (E,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0)) :: ((Place (F,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0, 0)) :: ((Place (G,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))), (Stdlib.Int.succ 0), (Stdlib.Int.succ
    0))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))), ((Place (A,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))),
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place
    (B, (Stdlib.Int.succ 0), 0, 0)) :: ((Place (C, 0, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: ((Place (D, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (E, (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ 0))) :: ((Place
    (F, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place
    (G, (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ 0))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))))), ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))), 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (B, (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place
    (C, (Stdlib.Int.succ 0), 0, (Stdlib.Int.succ 0))) :: ((Place (D,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (E, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ 0))) :: ((Place (F, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0, 0)) :: ((Place (G,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ 0),
    (Stdlib.Int.succ 0))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0)) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0, 0)) :: ((Place (B,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), 0)) :: ((Place (C, (Stdlib.Int.succ 0), 0,
    (Stdlib.Int.succ 0))) :: ((Place (D, 0, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ 0))) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))), (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))) :: ((Place (G, (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (H, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))), ((Place
    (A, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (B, 0, 0,
    0)) :: ((Place (C, 0, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    0)) :: ((Place (D, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (E,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (F, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ 0))) :: ((Place (G,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), 0)) :: ((Place (H, (Stdlib.Int.succ 0), 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))))))), ((Place
    (A, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (B, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ 0))) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), 0, 0)) :: ((Place (D, (Stdlib.Int.succ 0), (Stdlib.Int.succ 0),
    (Stdlib.Int.succ 0))) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0)) :: ((Place (F, 0, 0, (Stdlib.Int.succ 0))) :: ((Place (G,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))))))))), ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))), 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))) :: ((Place (B, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (C,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), 0, 0)) :: ((Place (D, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), 0)) :: ((Place (E, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), 0, (Stdlib.Int.succ 0))) :: ((Place (F,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))) :: ((Place (G, 0, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))),
    0)) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))))))))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))) :: ((Place (B, (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place (C,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (D, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ
    0)))) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0)) :: ((Place (F, 0, 0, (Stdlib.Int.succ 0))) :: ((Place (G,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))), 0,
    0)) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))))), ((Place (A,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))) :: ((Place (B, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (C, 0, 0, (Stdlib.Int.succ 0))) :: ((Place (D,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place
    (E, (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0)) :: ((Place
    (F, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0,
    0)) :: ((Place (G, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ
    0)))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ 0))) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))))))))))), ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (B,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), 0)) :: ((Place (C, 0, 0, 0)) :: ((Place (D, 0, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (E,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0)) :: ((Place
    (F, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (G, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (H,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ 0),
    0)) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))))))))))))), ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))), 0)) :: ((Place
    (B, (Stdlib.Int.succ 0), 0, 0)) :: ((Place (C, 0, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: ((Place (D, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place (G,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), 0)) :: ((Place (H, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    0)))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))))))))))))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), 0)) :: ((Place (B, (Stdlib.Int.succ 0), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (C,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    0)) :: ((Place (D, 0, (Stdlib.Int.succ 0), 0)) :: ((Place (E,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))) :: ((Place (F, (Stdlib.Int.succ 0), 0,
    0)) :: ((Place (G, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    0)))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))), (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))))))))), ((Place (A,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ 0))) :: ((Place
    (B, (Stdlib.Int.succ 0), 0, 0)) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    0)) :: ((Place (D, 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (E,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (F, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))) :: ((Place (G, 0, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    0)))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))))))))))))))), ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))), 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))) :: ((Place (B, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), 0)) :: ((Place (C, (Stdlib.Int.succ 0), 0, (Stdlib.Int.succ
    0))) :: ((Place (D, (Stdlib.Int.succ 0), 0, 0)) :: ((Place (E,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    0)), 0)) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place
    (G, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ 0))) :: ((Place
    (H, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))))))))))))))))), ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ 0))) :: ((Place
    (B, (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place (C, 0, 0,
    0)) :: ((Place (D, 0, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (E, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), 0)) :: ((Place (F, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ 0))) :: ((Place (G, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ 0),
    0)) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))))))))))))))))), ((Place (A, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (B,
    (Stdlib.Int.succ 0), 0, 0)) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))) :: ((Place (D, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place (E,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0)) :: ((Place (F,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (G,
    (Stdlib.Int.succ 0), 0, (Stdlib.Int.succ (Stdlib.Int.succ
    0)))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))))))))))))))))))), ((Place (A, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (B, (Stdlib.Int.succ
    0), 0, 0)) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place
    (D, 0, 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (F, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), (Stdlib.Int.succ 0))) :: ((Place (G,
    (Stdlib.Int.succ 0), 0, (Stdlib.Int.succ (Stdlib.Int.succ
    0)))) :: ((Place (H, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    0))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))))))))))))))))))), ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))), 0)) :: ((Place
    (B, (Stdlib.Int.succ 0), 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), 0, 0)) :: ((Place (D, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ 0), (Stdlib.Int.succ 0))) :: ((Place (F, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0)) :: ((Place (G, (Stdlib.Int.succ 0), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))))))))))))))))))))), ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), (Stdlib.Int.succ 0))) :: ((Place (B, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ 0), (Stdlib.Int.succ
    0))) :: ((Place (D, 0, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (E, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0)) :: ((Place (F, (Stdlib.Int.succ 0), 0, 0)) :: ((Place (G,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ 0), 0)) :: ((Place (H,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ
    0)))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))))))))))))))))))))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    0)) :: ((Place (B, 0, 0, 0)) :: ((Place (C, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (D, 0, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ 0))) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0)) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (G, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (H,
    (Stdlib.Int.succ 0), 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))))))))))))))))))))))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))), 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (B, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), 0)) :: ((Place (C, (Stdlib.Int.succ 0), 0,
    (Stdlib.Int.succ 0))) :: ((Place (D, (Stdlib.Int.succ 0), 0,
    0)) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0)) :: ((Place (F,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))) :: ((Place (G, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ 0))) :: ((Place (H, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))))))))))))))))))))))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), 0)) :: ((Place (B, (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place
    (C, 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ 0))) :: ((Place (D, 0, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (E, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), 0)) :: ((Place (F, 0, 0, 0)) :: ((Place (G,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    0), 0)) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))), (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))))))))))))))))))))))))), ((Place (A,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))), (Stdlib.Int.succ
    0))) :: ((Place (B, (Stdlib.Int.succ 0), 0, 0)) :: ((Place (C,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (D, 0, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (E,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    0)) :: ((Place (G, (Stdlib.Int.succ 0), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: ((Place (H, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    0))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))))))))))))))))))))))))), ((Place (A,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))) :: ((Place (B, (Stdlib.Int.succ 0), 0,
    0)) :: ((Place (C, 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: ((Place (D, (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0)) :: ((Place (E, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), 0)) :: ((Place (F, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (G, 0, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: ((Place (H, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    0)))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))))))))))))))))))))),
    ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))), 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (B, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (C, (Stdlib.Int.succ
    0), 0, (Stdlib.Int.succ 0))) :: ((Place (D, (Stdlib.Int.succ 0), 0,
    0)) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0)) :: ((Place (F,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))) :: ((Place (G, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ 0))) :: ((Place (H, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), 0)) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))))))))))))))))))))))))))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (B, (Stdlib.Int.succ
    0), 0, 0)) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place
    (D, 0, (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (E, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place
    (G, 0, 0, (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (H,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    0))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))))))))))))))))))))))))))))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), 0)) :: ((Place (B, 0, 0, 0)) :: ((Place (C,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    0)), 0)) :: ((Place (D, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: ((Place (E, (Stdlib.Int.succ 0), 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (F,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))) :: ((Place (G, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ 0))) :: ((Place (H,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))))))), (Stdlib.Int.succ 0)),
    ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))), 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (B, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (D, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), 0)) :: ((Place (E, (Stdlib.Int.succ 0), 0, 0)) :: ((Place (F,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))) :: ((Place (G, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ 0), 0)) :: ((Place (H,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), 0)) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))))))), (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))), 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (B, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (D, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place
    (E, 0, 0, 0)) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place
    (G, (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ 0),
    (Stdlib.Int.succ 0))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), 0)) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))), 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (B, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (D,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), 0)) :: ((Place (E, 0, 0, 0)) :: ((Place (F, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))) :: ((Place (G, (Stdlib.Int.succ 0), (Stdlib.Int.succ 0),
    (Stdlib.Int.succ 0))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), 0)) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))), ((Place (A,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (B,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    0)) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place
    (D, (Stdlib.Int.succ 0), 0, 0)) :: ((Place (E, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (F,
    (Stdlib.Int.succ 0), 0, (Stdlib.Int.succ 0))) :: ((Place (G,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), 0)) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))), ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place (B, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (D,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: ((Place (E, 0, 0, 0)) :: ((Place (F,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), 0)) :: ((Place (G, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ 0), (Stdlib.Int.succ 0))) :: ((Place (H,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ 0))) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))), ((Place (A, 0, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ 0))) :: ((Place (B, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ 0))) :: ((Place
    (C, (Stdlib.Int.succ 0), 0, (Stdlib.Int.succ (Stdlib.Int.succ
    0)))) :: ((Place (D, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (E,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0)) :: ((Place (F,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0)) :: ((Place (G,
    (Stdlib.Int.succ 0), 0, 0)) :: ((Place (H, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))))), ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: ((Place (B, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), 0)) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place (D,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place
    (E, 0, 0, 0)) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (G, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (H,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), (Stdlib.Int.succ 0), (Stdlib.Int.succ
    0))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))))), ((Place (A,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0, (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (B, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), 0)) :: ((Place (C, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0)) :: ((Place (D, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (E, 0, 0,
    0)) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place
    (G, (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))),
    (Stdlib.Int.succ 0), (Stdlib.Int.succ
    0))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))), ((Place
    (A, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0, (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (B, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), 0)) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))) :: ((Place (D, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ 0), (Stdlib.Int.succ 0))) :: ((Place (E, 0, 0,
    0)) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))), (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (G, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    0)) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))))))), ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: ((Place (B, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ 0))) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))), 0,
    0)) :: ((Place (D, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0)) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place
    (G, (Stdlib.Int.succ 0), 0, (Stdlib.Int.succ 0))) :: ((Place (H,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))), ((Place (A,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0, (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (B, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))), 0, 0)) :: ((Place (D,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place (E,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))), (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (G, (Stdlib.Int.succ 0), 0,
    (Stdlib.Int.succ 0))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))),
    0)) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))))))))), ((Place (A,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0, (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (B, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))), 0, 0)) :: ((Place (D,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place
    (E, (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0)) :: ((Place
    (F, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))) :: ((Place (G, (Stdlib.Int.succ 0), 0,
    (Stdlib.Int.succ 0))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    0))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))))),
    ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))), 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (B, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))), 0)) :: ((Place
    (C, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place (D,
    (Stdlib.Int.succ 0), 0, 0)) :: ((Place (E, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    0)) :: ((Place (F, (Stdlib.Int.succ 0), 0, (Stdlib.Int.succ
    0))) :: ((Place (G, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    0))) :: ((Place (H, 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))))))))))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (B, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place
    (C, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ 0), (Stdlib.Int.succ 0))) :: ((Place (D,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place
    (E, 0, 0, 0)) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place (G,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), 0)) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))))))), ((Place (A,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), 0)) :: ((Place (B, 0, 0, 0)) :: ((Place (C, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0)) :: ((Place (D,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), 0)) :: ((Place (E, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    0)))) :: ((Place (G, 0, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (H, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))))))))))))),
    ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))), 0)) :: ((Place (B, 0, 0,
    0)) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), 0)) :: ((Place (D, 0, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (E,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0,
    (Stdlib.Int.succ 0))) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (G, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))) :: ((Place (H, (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    0))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))))))))),
    ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))), 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (B, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (D, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ 0), 0)) :: ((Place (E,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0)) :: ((Place (F,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))) :: ((Place (G, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    0))) :: ((Place (H, 0, 0, 0)) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))))))))))))))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (B, (Stdlib.Int.succ
    0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0)) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))), 0,
    0)) :: ((Place (D, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (G, (Stdlib.Int.succ
    0), 0, (Stdlib.Int.succ 0))) :: ((Place (H, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))))))))))))))))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))), 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (B, (Stdlib.Int.succ
    0), (Stdlib.Int.succ 0), (Stdlib.Int.succ 0))) :: ((Place (C,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (D, (Stdlib.Int.succ
    0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    0)) :: ((Place (E, 0, 0, 0)) :: ((Place (F, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))) :: ((Place (G, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), 0)) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))))))))))))))))), ((Place (A, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (B, 0, 0,
    0)) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))) :: ((Place (D, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ 0))) :: ((Place (E, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), 0)) :: ((Place (F, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (G, 0, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (H,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    0)), 0)) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))))))))))))))))))), ((Place (A, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (B, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ 0))) :: ((Place (D, 0, 0, (Stdlib.Int.succ
    0))) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0)) :: ((Place (F, (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place (G, 0, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (H,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), 0, 0)) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))))))))))))))))))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), 0)) :: ((Place (B, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (C,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ 0),
    0)) :: ((Place (D, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    0)))) :: ((Place (E, (Stdlib.Int.succ 0), 0, 0)) :: ((Place (F,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ 0))) :: ((Place (G, 0, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (H, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))))))))))))))))))))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), (Stdlib.Int.succ 0))) :: ((Place (B,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))) :: ((Place (C, (Stdlib.Int.succ 0), 0,
    0)) :: ((Place (D, 0, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (E, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0)) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    0)) :: ((Place (G, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (H, 0, (Stdlib.Int.succ 0),
    0)) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))))))))))))))))))))), ((Place (A,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    0)), 0)) :: ((Place (B, 0, (Stdlib.Int.succ 0), (Stdlib.Int.succ
    0))) :: ((Place (C, 0, 0, 0)) :: ((Place (D, 0, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (E,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0)) :: ((Place (F,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))) :: ((Place (G, (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))) :: ((Place (H, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))))))))))))))))))))))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (B, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), 0)) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place
    (D, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (E, 0, 0, 0)) :: ((Place (F, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))) :: ((Place (G, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (H,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), (Stdlib.Int.succ 0), (Stdlib.Int.succ
    0))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))))))))))))))))))))))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))), 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (B, (Stdlib.Int.succ
    0), (Stdlib.Int.succ 0), (Stdlib.Int.succ 0))) :: ((Place (C,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (D, (Stdlib.Int.succ
    0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    0)) :: ((Place (E, 0, 0, 0)) :: ((Place (F, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))) :: ((Place (G, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (H, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))),
    0)) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))))))))))))))))))))))))), ((Place (A,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0, 0)) :: ((Place (B, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), 0)) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))) :: ((Place (D, 0, 0, (Stdlib.Int.succ
    0))) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0)) :: ((Place (F,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    0)))) :: ((Place (G, 0, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (H, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))))))))))))))))))))))))), ((Place
    (A, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0, (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (B, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (C,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), 0, 0)) :: ((Place (D, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), 0)) :: ((Place (E, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))), (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (G, (Stdlib.Int.succ 0), 0,
    (Stdlib.Int.succ 0))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))),
    0)) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))))))))))))))))))))))))))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (B, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), 0, 0)) :: ((Place (D, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), 0)) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))), (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (G, (Stdlib.Int.succ 0), 0,
    (Stdlib.Int.succ 0))) :: ((Place (H, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))),
    (Stdlib.Int.succ 0))) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))))))))))))))))))))))))))), ((Place (A,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), 0)) :: ((Place (B, 0, 0, 0)) :: ((Place (C, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0)) :: ((Place (D, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (E,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    0))) :: ((Place (G, 0, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))), (Stdlib.Int.succ 0)),
    ((Place (A, (Stdlib.Int.succ 0), (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (B, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), 0)) :: ((Place (C, 0, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: ((Place (D, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    0)) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    0))) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (G, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (H, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0,
    0)) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: ((Place (B, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))), (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (D, (Stdlib.Int.succ
    0), (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0)) :: ((Place (E,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0)) :: ((Place (F,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))),
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place
    (G, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ 0))) :: ((Place (H, 0, 0,
    0)) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: ((Place (B, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), 0)) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))),
    (Stdlib.Int.succ 0), 0)) :: ((Place (D, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (E, (Stdlib.Int.succ
    0), (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (F,
    0, 0, 0)) :: ((Place (G, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ 0))) :: ((Place (H, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ
    0)))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))), ((Place (A,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0, (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (B, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (D,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (E,
    0, 0, 0)) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), 0)) :: ((Place (G, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ 0))) :: ((Place (H, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), (Stdlib.Int.succ 0), (Stdlib.Int.succ
    0))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))), ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))), 0, (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (B,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), 0)) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))), (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))) :: ((Place (D, (Stdlib.Int.succ 0), (Stdlib.Int.succ 0),
    0)) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0)) :: ((Place (F, 0, 0, 0)) :: ((Place (G, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place
    (H, 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: ((Place (B, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (C, 0, 0, 0)) :: ((Place (D, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place (E,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0)) :: ((Place (F,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ 0), 0)) :: ((Place (G, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    0)))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (B, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ 0), (Stdlib.Int.succ
    0))) :: ((Place (D, (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), 0)) :: ((Place (E, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), 0)) :: ((Place (F, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (G, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ 0))) :: ((Place (H, 0, 0,
    0)) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))))), ((Place
    (A, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0, (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (B, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), 0)) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))) :: ((Place (D, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ 0), (Stdlib.Int.succ 0))) :: ((Place (E, 0, 0,
    0)) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (G, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (H, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), 0)) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))))))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (B, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), 0)) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (D, (Stdlib.Int.succ
    0), (Stdlib.Int.succ 0), (Stdlib.Int.succ 0))) :: ((Place (E, 0, 0,
    0)) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (G, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), 0)) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))))))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (B, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), 0)) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))) :: ((Place (D, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ 0), (Stdlib.Int.succ 0))) :: ((Place (E, 0, 0,
    0)) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (G, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (H, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), 0)) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))), ((Place (A,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0, (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (B, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))), 0, 0)) :: ((Place (D,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place (E,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (G, (Stdlib.Int.succ
    0), 0, (Stdlib.Int.succ 0))) :: ((Place (H, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))),
    0)) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))))))))),
    ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), 0, (Stdlib.Int.succ (Stdlib.Int.succ
    0)))) :: ((Place (B, (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place (C,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (D, (Stdlib.Int.succ 0), (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0)) :: ((Place (F, (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), 0)) :: ((Place (G, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (H, 0, 0, 0)) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))))))))))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (B, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))), 0,
    0)) :: ((Place (D, (Stdlib.Int.succ 0), (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0)) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (G, (Stdlib.Int.succ 0), 0,
    (Stdlib.Int.succ 0))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    0))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))))))))))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (B, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))), 0,
    0)) :: ((Place (D, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    0)) :: ((Place (E, 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (G, (Stdlib.Int.succ
    0), 0, (Stdlib.Int.succ 0))) :: ((Place (H, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))),
    0)) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))))))), ((Place (A,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0, (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (B, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), 0)) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (D, (Stdlib.Int.succ 0), (Stdlib.Int.succ 0),
    (Stdlib.Int.succ 0))) :: ((Place (E, 0, 0, 0)) :: ((Place (F,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (G, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place
    (H, (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), 0)) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))))))))))))), ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: ((Place (B, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), 0)) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))), (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (D, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ 0), (Stdlib.Int.succ 0))) :: ((Place (E, 0, 0,
    0)) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (G, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place (H,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))))))))))))))), ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: ((Place (B, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), 0)) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (D,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ 0), (Stdlib.Int.succ
    0))) :: ((Place (E, 0, 0, 0)) :: ((Place (F, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (G, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place
    (H, 0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    0)) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))))))))))))))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (B, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))), 0,
    0)) :: ((Place (D, (Stdlib.Int.succ 0), (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0)) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place
    (G, (Stdlib.Int.succ 0), 0, (Stdlib.Int.succ 0))) :: ((Place (H,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    0))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))))))))))), ((Place (A,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0, (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (B, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ 0))) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), 0, 0)) :: ((Place (D, (Stdlib.Int.succ 0), (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0)) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place
    (G, (Stdlib.Int.succ 0), 0, (Stdlib.Int.succ 0))) :: ((Place (H,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))))))))))))))))), ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: ((Place (B, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ 0))) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), 0)) :: ((Place (D, (Stdlib.Int.succ 0), (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (E, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (F, (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), 0)) :: ((Place (G, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (H,
    0, 0, 0)) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))))))))))))))))))), ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: ((Place (B, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ 0))) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))), 0,
    0)) :: ((Place (D, (Stdlib.Int.succ 0), (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0)) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place
    (G, (Stdlib.Int.succ 0), 0, (Stdlib.Int.succ 0))) :: ((Place (H,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))))))))))))))))))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), 0)) :: ((Place (B, (Stdlib.Int.succ 0), (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (C,
    0, 0, (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (D, 0, 0,
    0)) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ 0), 0)) :: ((Place (F, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0)) :: ((Place
    (G, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))))))))))))))))))))), ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0, (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: ((Place (B, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place (C,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (D,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ 0), (Stdlib.Int.succ
    0))) :: ((Place (E, 0, 0, 0)) :: ((Place (F, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (G,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), 0)) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))))))))))))))))))))), ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))), 0, (Stdlib.Int.succ (Stdlib.Int.succ
    0)))) :: ((Place (B, (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place (C, 0, 0,
    0)) :: ((Place (D, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ 0))) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0)) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (G,
    0, (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: ((Place (H, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ 0),
    0)) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))))))))))))))))))))))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (B, (Stdlib.Int.succ
    0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0)) :: ((Place (C, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))), 0,
    0)) :: ((Place (D, (Stdlib.Int.succ 0), (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (E, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place
    (G, (Stdlib.Int.succ 0), 0, (Stdlib.Int.succ 0))) :: ((Place (H,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))))))))))))))))))))))), ((Place (A,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0, (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (B, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ 0))) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), 0, 0)) :: ((Place (D, (Stdlib.Int.succ 0), (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0)) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place
    (G, (Stdlib.Int.succ 0), 0, (Stdlib.Int.succ 0))) :: ((Place (H,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))))))))))))))))))), ((Place
    (A, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0, (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (B,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), (Stdlib.Int.succ 0))) :: ((Place (C,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), 0)) :: ((Place (D, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))) :: ((Place (E, 0, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))) :: ((Place (F, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), 0)) :: ((Place (G, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))) :: ((Place (H, 0, 0,
    0)) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))))))))))))))))))))))))), ((Place (A, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))), 0,
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (B, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ 0))) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), 0, 0)) :: ((Place (D, (Stdlib.Int.succ 0), (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (E, (Stdlib.Int.succ (Stdlib.Int.succ 0)),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0)) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place
    (G, (Stdlib.Int.succ 0), 0, (Stdlib.Int.succ 0))) :: ((Place (H, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))) :: []))))))))) :: ((((Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))))))))))))))))))))))))))))), ((Place (A,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0, (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (B, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place
    (C, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))))) :: ((Place
    (D, (Stdlib.Int.succ 0), (Stdlib.Int.succ 0), (Stdlib.Int.succ
    0))) :: ((Place (E, 0, 0, 0)) :: ((Place (F, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))))) :: ((Place (G, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))) :: ((Place (H, (Stdlib.Int.succ 0), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))), 0)) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))))))))))))))))))))))))))), ((Place (A,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0)))), 0, (Stdlib.Int.succ (Stdlib.Int.succ 0)))) :: ((Place (B, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))), 0)) :: ((Place
    (C, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))), (Stdlib.Int.succ 0),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (D,
    (Stdlib.Int.succ 0), (Stdlib.Int.succ 0), (Stdlib.Int.succ
    0))) :: ((Place (E, 0, 0, 0)) :: ((Place (F, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (G,
    0, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))), 0)) :: ((Place (H, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ 0)))),
    (Stdlib.Int.succ (Stdlib.Int.succ 0)), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))))))) :: []))))))))) :: ((((Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))))))))))))))))))))))))))))),
    ((Place (A, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0)))), 0, (Stdlib.Int.succ (Stdlib.Int.succ
    0)))) :: ((Place (B, 0, (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))), 0)) :: ((Place (C, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    0)), (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))))) :: ((Place (D, (Stdlib.Int.succ 0),
    (Stdlib.Int.succ 0), (Stdlib.Int.succ 0))) :: ((Place (E, 0, 0,
    0)) :: ((Place (F, (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    0))), (Stdlib.Int.succ 0), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))))) :: ((Place (G, (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))), (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0)))), (Stdlib.Int.succ
    (Stdlib.Int.succ (Stdlib.Int.succ 0))))) :: ((Place (H, 0,
    (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ (Stdlib.Int.succ
    (Stdlib.Int.succ 0))))),
    0)) :: []))))))))) :: [])))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

(** val date_eqb : (int * int) -> (int * int) -> bool **)

let date_eqb x y =
  (&&) (eqb (fst x) (fst y)) (eqb (snd x) (snd y))

(** val lookup_solution :
    int -> int -> ((int * int) * placement list) list -> placement list option **)

let rec lookup_solution month day = function
| [] -> None
| p :: rest ->
  let (p0, cert) = p in
  if date_eqb (month, day) p0
  then Some cert
  else lookup_solution month day rest

(** val certified_dateb : (int * int) -> bool **)

let certified_dateb = function
| (month, day) ->
  (match lookup_solution month day solution_table with
   | Some cert -> verify month day cert
   | None -> false)

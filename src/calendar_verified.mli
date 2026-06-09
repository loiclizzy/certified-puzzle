
val negb : bool -> bool

val fst : ('a1 * 'a2) -> 'a1

val snd : ('a1 * 'a2) -> 'a2

val length : 'a1 list -> int

val app : 'a1 list -> 'a1 list -> 'a1 list

val add : int -> int -> int

val sub : int -> int -> int

val eqb : int -> int -> bool

module Nat :
 sig
 end

val map : ('a1 -> 'a2) -> 'a1 list -> 'a2 list

val nth_error : 'a1 list -> int -> 'a1 option

val existsb : ('a1 -> bool) -> 'a1 list -> bool

val forallb : ('a1 -> bool) -> 'a1 list -> bool

type cell = int * int

val xy : int -> int -> cell

val cell_eqb : cell -> cell -> bool

type piece_id =
| A
| B
| C
| D
| E
| F
| G
| H

val piece_id_eq_dec : piece_id -> piece_id -> bool

val piece_eqb : piece_id -> piece_id -> bool

val all_pieces : piece_id list

val board_cells : cell list

val month_cells : cell list

val day_cells : cell list

val target_cells : int -> int -> (cell * cell) option

type placement =
| Place of piece_id * int * int * int

val placement_piece : placement -> piece_id

val placement_orientation : placement -> int

val placement_row : placement -> int

val placement_col : placement -> int

type shape = cell list

type variants = shape list

val variants_A : variants

val variants_B : variants

val variants_C : variants

val variants_D : variants

val variants_E : variants

val variants_F : variants

val variants_G : variants

val variants_H : variants

val variants_of : piece_id -> variants

val translate : int -> int -> shape -> cell list

val placement_cells : placement -> cell list option

val collect_cells : placement list -> cell list option

val nodupb : ('a1 -> 'a1 -> bool) -> 'a1 list -> bool

val in_boardb : cell -> bool

val pieces_presentb : piece_id list -> bool

val verify : int -> int -> placement list -> bool

val solution_table : ((int * int) * placement list) list

val date_eqb : (int * int) -> (int * int) -> bool

val lookup_solution :
  int -> int -> ((int * int) * placement list) list -> placement list option

val certified_dateb : (int * int) -> bool

From Stdlib Require Import List.
Import ListNotations.
Require Import CalendarPuzzle.

(** AUTO-GENERATED FILE.

    The external solver only guessed these certificates.
    Rocq does not trust the solver: every entry is checked again by [verify].
*)

Definition solution_table :
  list ((nat * nat) * list placement) :=
  [((1, 1),
     [Place A 4 0 0;
       Place B 0 4 4;
       Place C 4 2 3;
       Place D 0 0 4;
       Place E 2 4 0;
       Place F 5 2 0;
       Place G 0 1 2;
       Place H 5 3 2]);
   ((1, 2),
     [Place A 4 0 0;
       Place B 0 1 2;
       Place C 4 2 0;
       Place D 0 0 4;
       Place E 1 3 4;
       Place F 0 4 0;
       Place G 3 5 0;
       Place H 6 4 3]);
   ((1, 3),
     [Place A 7 5 0;
       Place B 1 0 4;
       Place C 5 2 0;
       Place D 0 1 0;
       Place E 3 2 1;
       Place F 6 2 5;
       Place G 1 3 4;
       Place H 1 0 1]);
   ((1, 4),
     [Place A 5 3 0;
       Place B 0 4 4;
       Place C 1 0 2;
       Place D 1 1 4;
       Place E 0 3 3;
       Place F 4 0 0;
       Place G 0 2 0;
       Place H 7 4 1]);
   ((1, 5),
     [Place A 3 0 5;
       Place B 0 4 1;
       Place C 1 0 1;
       Place D 1 2 3;
       Place E 2 4 0;
       Place F 1 1 0;
       Place G 3 4 4;
       Place H 2 2 0]);
   ((1, 6),
     [Place A 4 0 0;
       Place B 0 4 4;
       Place C 7 2 3;
       Place D 1 2 0;
       Place E 2 4 0;
       Place F 4 1 1;
       Place G 3 4 1;
       Place H 6 0 3]);
   ((1, 7),
     [Place A 2 2 5;
       Place B 0 0 3;
       Place C 3 0 1;
       Place D 0 2 3;
       Place E 2 3 3;
       Place F 3 3 0;
       Place G 2 4 1;
       Place H 4 1 0]);
   ((1, 8),
     [Place A 5 3 0;
       Place B 0 0 1;
       Place C 4 1 0;
       Place D 0 0 4;
       Place E 3 1 2;
       Place F 1 4 2;
       Place G 2 3 5;
       Place H 0 5 1]);
   ((1, 9),
     [Place A 7 5 0;
       Place B 0 0 1;
       Place C 4 1 0;
       Place D 0 0 4;
       Place E 3 1 2;
       Place F 1 4 2;
       Place G 2 3 5;
       Place H 5 3 0]);
   ((1, 10),
     [Place A 6 3 0;
       Place B 1 1 3;
       Place C 2 1 0;
       Place D 2 4 1;
       Place E 1 0 3;
       Place F 4 4 3;
       Place G 2 0 1;
       Place H 7 2 5]);
   ((1, 11),
     [Place A 7 5 0;
       Place B 0 0 3;
       Place C 1 4 1;
       Place D 0 3 4;
       Place E 1 2 4;
       Place F 1 2 0;
       Place G 1 3 0;
       Place H 6 0 0]);
   ((1, 12),
     [Place A 0 0 1;
       Place B 1 3 5;
       Place C 4 4 1;
       Place D 0 0 4;
       Place E 2 4 0;
       Place F 6 1 1;
       Place G 1 2 3;
       Place H 3 1 0]);
   ((1, 13),
     [Place A 3 0 5;
       Place B 0 4 4;
       Place C 1 0 1;
       Place D 1 2 0;
       Place E 2 4 0;
       Place F 0 1 0;
       Place G 3 4 1;
       Place H 1 2 2]);
   ((1, 14),
     [Place A 5 0 0;
       Place B 0 4 4;
       Place C 0 2 1;
       Place D 0 0 4;
       Place E 0 3 3;
       Place F 6 3 1;
       Place G 1 4 0;
       Place H 2 0 2]);
   ((1, 15),
     [Place A 6 3 0;
       Place B 0 0 3;
       Place C 6 3 1;
       Place D 0 0 1;
       Place E 1 2 4;
       Place F 4 4 3;
       Place G 0 3 3;
       Place H 3 1 0]);
   ((1, 16),
     [Place A 7 5 0;
       Place B 0 1 0;
       Place C 6 0 3;
       Place D 1 3 2;
       Place E 1 0 1;
       Place F 3 0 5;
       Place G 1 3 0;
       Place H 7 3 5]);
   ((1, 17),
     [Place A 3 3 1;
       Place B 0 4 4;
       Place C 5 3 0;
       Place D 1 1 4;
       Place E 0 3 3;
       Place F 1 2 0;
       Place G 0 0 3;
       Place H 6 0 0]);
   ((1, 18),
     [Place A 1 1 3;
       Place B 1 3 5;
       Place C 1 0 2;
       Place D 0 3 0;
       Place E 2 4 0;
       Place F 2 1 2;
       Place G 2 3 3;
       Place H 7 0 0]);
   ((1, 19),
     [Place A 1 1 3;
       Place B 1 3 5;
       Place C 1 0 2;
       Place D 0 3 0;
       Place E 2 4 0;
       Place F 2 1 2;
       Place G 1 3 3;
       Place H 7 0 0]);
   ((1, 20),
     [Place A 1 4 2;
       Place B 0 5 0;
       Place C 3 2 5;
       Place D 3 2 1;
       Place E 0 2 0;
       Place F 3 1 4;
       Place G 0 0 3;
       Place H 6 0 0]);
   ((1, 21),
     [Place A 0 4 3;
       Place B 1 0 4;
       Place C 7 2 3;
       Place D 1 2 0;
       Place E 3 0 1;
       Place F 0 5 1;
       Place G 1 4 0;
       Place H 6 0 0]);
   ((1, 22),
     [Place A 7 5 0;
       Place B 1 0 1;
       Place C 0 4 1;
       Place D 2 1 2;
       Place E 1 0 3;
       Place F 3 1 0;
       Place G 3 2 4;
       Place H 6 4 4]);
   ((1, 23),
     [Place A 4 5 1;
       Place B 0 0 3;
       Place C 6 2 5;
       Place D 0 0 1;
       Place E 0 4 0;
       Place F 1 3 1;
       Place G 2 2 4;
       Place H 3 1 0]);
   ((1, 24),
     [Place A 2 2 0;
       Place B 0 0 3;
       Place C 0 2 3;
       Place D 0 2 1;
       Place E 2 4 0;
       Place F 7 4 3;
       Place G 0 3 4;
       Place H 6 0 0]);
   ((1, 25),
     [Place A 6 3 0;
       Place B 1 3 5;
       Place C 2 1 0;
       Place D 2 4 1;
       Place E 1 3 2;
       Place F 7 1 3;
       Place G 2 0 1;
       Place H 0 0 3]);
   ((1, 26),
     [Place A 4 2 3;
       Place B 1 3 5;
       Place C 0 0 1;
       Place D 3 0 3;
       Place E 0 3 0;
       Place F 1 4 1;
       Place G 3 1 0;
       Place H 6 5 0]);
   ((1, 27),
     [Place A 3 1 0;
       Place B 0 3 3;
       Place C 3 2 5;
       Place D 3 1 2;
       Place E 2 4 0;
       Place F 4 4 1;
       Place G 0 0 3;
       Place H 3 0 1]);
   ((1, 28),
     [Place A 6 3 0;
       Place B 0 0 3;
       Place C 2 1 0;
       Place D 2 4 1;
       Place E 1 2 4;
       Place F 4 2 2;
       Place G 2 0 1;
       Place H 6 4 3]);
   ((1, 29),
     [Place A 3 3 0;
       Place B 0 3 1;
       Place C 0 5 2;
       Place D 3 0 3;
       Place E 0 0 2;
       Place F 5 2 5;
       Place G 1 2 4;
       Place H 7 0 0]);
   ((1, 30),
     [Place A 5 3 0;
       Place B 0 4 4;
       Place C 0 2 0;
       Place D 1 1 4;
       Place E 0 3 3;
       Place F 6 3 1;
       Place G 0 0 3;
       Place H 6 0 0]);
   ((1, 31),
     [Place A 5 3 0;
       Place B 1 3 5;
       Place C 0 5 1;
       Place D 0 0 4;
       Place E 3 2 2;
       Place F 2 0 3;
       Place G 0 2 0;
       Place H 6 0 0]);
   ((2, 1),
     [Place A 2 0 0;
       Place B 0 0 2;
       Place C 5 3 0;
       Place D 1 2 2;
       Place E 3 0 3;
       Place F 5 2 5;
       Place G 3 4 3;
       Place H 4 4 1]);
   ((2, 2),
     [Place A 4 0 1;
       Place B 1 2 5;
       Place C 5 0 0;
       Place D 3 0 3;
       Place E 2 4 0;
       Place F 4 4 3;
       Place G 3 2 2;
       Place H 0 4 1]);
   ((2, 3),
     [Place A 3 0 0;
       Place B 0 5 0;
       Place C 2 2 3;
       Place D 3 2 4;
       Place E 1 0 3;
       Place F 4 0 1;
       Place G 3 3 0;
       Place H 6 4 4]);
   ((2, 4),
     [Place A 2 0 0;
       Place B 0 5 0;
       Place C 0 0 2;
       Place D 1 2 2;
       Place E 2 2 0;
       Place F 5 2 5;
       Place G 3 4 3;
       Place H 1 1 3]);
   ((2, 5),
     [Place A 2 0 0;
       Place B 1 2 5;
       Place C 4 4 3;
       Place D 3 0 3;
       Place E 0 0 2;
       Place F 3 2 0;
       Place G 0 3 2;
       Place H 6 5 0]);
   ((2, 6),
     [Place A 2 0 0;
       Place B 0 5 0;
       Place C 1 0 2;
       Place D 3 2 4;
       Place E 2 2 0;
       Place F 5 2 2;
       Place G 0 1 2;
       Place H 6 4 4]);
   ((2, 7),
     [Place A 2 0 0;
       Place B 0 5 0;
       Place C 5 0 5;
       Place D 0 3 2;
       Place E 2 2 0;
       Place F 5 0 3;
       Place G 1 0 2;
       Place H 1 4 4]);
   ((2, 8),
     [Place A 3 0 0;
       Place B 0 4 1;
       Place C 1 0 2;
       Place D 0 1 1;
       Place E 2 4 0;
       Place F 6 2 5;
       Place G 1 3 4;
       Place H 2 1 3]);
   ((2, 9),
     [Place A 2 1 5;
       Place B 0 5 0;
       Place C 4 0 0;
       Place D 0 3 4;
       Place E 0 2 2;
       Place F 1 0 2;
       Place G 1 2 0;
       Place H 5 3 3]);
   ((2, 10),
     [Place A 2 0 0;
       Place B 0 5 0;
       Place C 3 2 2;
       Place D 0 0 4;
       Place E 2 2 0;
       Place F 3 2 4;
       Place G 2 3 5;
       Place H 2 0 2]);
   ((2, 11),
     [Place A 2 0 0;
       Place B 0 5 0;
       Place C 4 4 3;
       Place D 3 1 2;
       Place E 2 2 0;
       Place F 3 0 5;
       Place G 3 3 4;
       Place H 0 0 2]);
   ((2, 12),
     [Place A 3 0 0;
       Place B 0 0 3;
       Place C 4 4 3;
       Place D 1 3 0;
       Place E 3 2 4;
       Place F 5 0 1;
       Place G 0 2 3;
       Place H 2 5 0]);
   ((2, 13),
     [Place A 2 0 0;
       Place B 0 5 0;
       Place C 7 4 3;
       Place D 1 1 4;
       Place E 2 2 0;
       Place F 3 0 2;
       Place G 0 0 3;
       Place H 2 3 3]);
   ((2, 14),
     [Place A 3 0 0;
       Place B 0 0 3;
       Place C 2 2 3;
       Place D 1 3 0;
       Place E 2 3 4;
       Place F 5 0 1;
       Place G 1 2 5;
       Place H 2 5 0]);
   ((2, 15),
     [Place A 2 2 3;
       Place B 0 5 0;
       Place C 4 0 0;
       Place D 1 3 1;
       Place E 1 0 2;
       Place F 3 0 5;
       Place G 0 2 0;
       Place H 7 3 5]);
   ((2, 16),
     [Place A 2 0 0;
       Place B 1 3 3;
       Place C 5 2 0;
       Place D 1 1 4;
       Place E 3 4 0;
       Place F 3 0 2;
       Place G 0 0 3;
       Place H 5 3 5]);
   ((2, 17),
     [Place A 4 2 1;
       Place B 0 5 0;
       Place C 4 0 0;
       Place D 1 3 4;
       Place E 2 3 3;
       Place F 1 0 2;
       Place G 1 2 0;
       Place H 5 1 5]);
   ((2, 18),
     [Place A 2 0 0;
       Place B 0 5 0;
       Place C 7 4 3;
       Place D 0 2 3;
       Place E 2 2 0;
       Place F 1 0 2;
       Place G 1 1 2;
       Place H 5 1 5]);
   ((2, 19),
     [Place A 3 0 0;
       Place B 0 0 3;
       Place C 6 2 5;
       Place D 1 3 0;
       Place E 1 2 3;
       Place F 5 0 1;
       Place G 1 3 3;
       Place H 2 5 0]);
   ((2, 20),
     [Place A 2 0 0;
       Place B 0 5 0;
       Place C 7 4 3;
       Place D 1 1 4;
       Place E 2 2 0;
       Place F 3 0 2;
       Place G 0 0 3;
       Place H 0 3 3]);
   ((2, 21),
     [Place A 0 4 3;
       Place B 0 0 3;
       Place C 4 4 1;
       Place D 1 2 0;
       Place E 2 4 0;
       Place F 1 2 1;
       Place G 3 0 0;
       Place H 6 2 4]);
   ((2, 22),
     [Place A 7 5 0;
       Place B 0 4 4;
       Place C 5 0 0;
       Place D 2 1 2;
       Place E 1 0 3;
       Place F 0 4 0;
       Place G 3 2 4;
       Place H 7 0 1]);
   ((2, 23),
     [Place A 4 5 1;
       Place B 0 3 3;
       Place C 6 2 5;
       Place D 1 0 0;
       Place E 0 4 0;
       Place F 1 0 2;
       Place G 3 1 3;
       Place H 2 2 0]);
   ((2, 24),
     [Place A 6 0 1;
       Place B 1 0 4;
       Place C 6 0 2;
       Place D 3 2 4;
       Place E 2 4 0;
       Place F 2 0 0;
       Place G 0 4 1;
       Place H 6 4 4]);
   ((2, 25),
     [Place A 2 0 0;
       Place B 0 4 4;
       Place C 7 2 3;
       Place D 1 0 2;
       Place E 1 0 3;
       Place F 3 2 0;
       Place G 1 2 2;
       Place H 6 5 0]);
   ((2, 26),
     [Place A 2 0 0;
       Place B 0 5 0;
       Place C 5 1 4;
       Place D 0 0 4;
       Place E 2 2 0;
       Place F 5 2 2;
       Place G 2 3 5;
       Place H 3 0 2]);
   ((2, 27),
     [Place A 7 5 0;
       Place B 0 0 3;
       Place C 3 2 5;
       Place D 2 3 0;
       Place E 0 2 0;
       Place F 1 4 2;
       Place G 3 0 0;
       Place H 2 2 3]);
   ((2, 28),
     [Place A 3 0 0;
       Place B 0 5 0;
       Place C 1 0 2;
       Place D 0 2 3;
       Place E 2 3 3;
       Place F 0 1 1;
       Place G 3 3 0;
       Place H 4 2 5]);
   ((2, 29),
     [Place A 2 0 0;
       Place B 0 0 2;
       Place C 5 2 0;
       Place D 0 3 2;
       Place E 1 2 2;
       Place F 3 0 5;
       Place G 2 4 1;
       Place H 7 3 5]);
   ((3, 1),
     [Place A 1 2 1;
       Place B 1 4 1;
       Place C 2 3 0;
       Place D 3 3 3;
       Place E 3 3 4;
       Place F 4 1 3;
       Place G 0 0 3;
       Place H 2 0 0]);
   ((3, 2),
     [Place A 1 0 0;
       Place B 1 4 0;
       Place C 3 0 3;
       Place D 3 4 2;
       Place E 2 1 0;
       Place F 3 0 5;
       Place G 2 2 2;
       Place H 7 3 5]);
   ((3, 3),
     [Place A 3 0 0;
       Place B 0 4 4;
       Place C 4 0 1;
       Place D 1 3 0;
       Place E 1 0 3;
       Place F 5 2 2;
       Place G 3 2 4;
       Place H 2 5 0]);
   ((3, 4),
     [Place A 1 0 0;
       Place B 1 4 1;
       Place C 2 3 0;
       Place D 1 1 0;
       Place E 1 2 4;
       Place F 4 4 3;
       Place G 0 3 3;
       Place H 1 0 3]);
   ((3, 5),
     [Place A 1 0 0;
       Place B 1 3 0;
       Place C 3 2 5;
       Place D 1 1 0;
       Place E 3 4 0;
       Place F 3 2 3;
       Place G 2 3 4;
       Place H 1 0 3]);
   ((3, 6),
     [Place A 3 1 4;
       Place B 1 0 0;
       Place C 6 2 5;
       Place D 0 3 0;
       Place E 2 4 0;
       Place F 2 1 2;
       Place G 0 0 3;
       Place H 5 3 3]);
   ((3, 7),
     [Place A 1 0 0;
       Place B 0 5 0;
       Place C 7 4 3;
       Place D 3 1 2;
       Place E 1 0 3;
       Place F 2 1 0;
       Place G 3 3 1;
       Place H 0 3 4]);
   ((3, 8),
     [Place A 1 0 0;
       Place B 0 5 0;
       Place C 6 2 5;
       Place D 1 1 0;
       Place E 1 2 3;
       Place F 4 3 0;
       Place G 2 3 3;
       Place H 1 0 3]);
   ((3, 9),
     [Place A 1 0 0;
       Place B 1 3 5;
       Place C 4 1 0;
       Place D 0 3 2;
       Place E 0 2 4;
       Place F 3 3 0;
       Place G 2 4 1;
       Place H 1 0 3]);
   ((3, 10),
     [Place A 3 0 0;
       Place B 0 4 4;
       Place C 3 2 2;
       Place D 0 0 4;
       Place E 2 1 4;
       Place F 2 3 0;
       Place G 3 0 1;
       Place H 7 4 1]);
   ((3, 11),
     [Place A 1 0 0;
       Place B 0 5 0;
       Place C 1 4 0;
       Place D 3 1 2;
       Place E 1 0 3;
       Place F 6 2 5;
       Place G 1 3 4;
       Place H 5 1 0]);
   ((3, 12),
     [Place A 1 0 0;
       Place B 0 5 0;
       Place C 0 2 3;
       Place D 3 3 3;
       Place E 3 3 4;
       Place F 3 1 0;
       Place G 2 2 1;
       Place H 1 0 3]);
   ((3, 13),
     [Place A 3 0 5;
       Place B 0 4 4;
       Place C 2 1 2;
       Place D 2 3 2;
       Place E 2 4 0;
       Place F 5 2 0;
       Place G 2 0 3;
       Place H 3 0 0]);
   ((3, 14),
     [Place A 1 4 3;
       Place B 1 4 0;
       Place C 2 0 0;
       Place D 2 1 3;
       Place E 0 0 3;
       Place F 3 3 2;
       Place G 1 2 5;
       Place H 3 1 1]);
   ((3, 15),
     [Place A 6 3 0;
       Place B 1 2 5;
       Place C 2 0 0;
       Place D 2 4 1;
       Place E 3 1 2;
       Place F 4 4 3;
       Place G 0 0 3;
       Place H 2 1 1]);
   ((3, 16),
     [Place A 7 5 0;
       Place B 1 0 0;
       Place C 6 2 5;
       Place D 0 1 3;
       Place E 1 0 3;
       Place F 1 4 2;
       Place G 1 3 0;
       Place H 5 1 2]);
   ((3, 17),
     [Place A 1 0 0;
       Place B 0 3 3;
       Place C 6 2 5;
       Place D 3 1 2;
       Place E 1 0 3;
       Place F 0 5 1;
       Place G 1 4 0;
       Place H 5 1 0]);
   ((3, 18),
     [Place A 1 0 0;
       Place B 1 3 0;
       Place C 0 2 3;
       Place D 1 1 0;
       Place E 3 4 0;
       Place F 7 4 3;
       Place G 0 3 4;
       Place H 1 0 3]);
   ((3, 19),
     [Place A 1 0 0;
       Place B 1 4 1;
       Place C 2 3 0;
       Place D 1 1 0;
       Place E 1 0 3;
       Place F 5 2 5;
       Place G 3 4 3;
       Place H 7 1 3]);
   ((3, 20),
     [Place A 1 4 2;
       Place B 1 0 0;
       Place C 3 2 5;
       Place D 0 3 0;
       Place E 2 4 0;
       Place F 3 1 4;
       Place G 0 0 3;
       Place H 5 1 2]);
   ((3, 21),
     [Place A 0 4 3;
       Place B 1 0 4;
       Place C 7 2 3;
       Place D 1 2 0;
       Place E 3 0 1;
       Place F 0 5 1;
       Place G 1 4 0;
       Place H 2 0 0]);
   ((3, 22),
     [Place A 7 5 0;
       Place B 1 0 0;
       Place C 7 2 0;
       Place D 3 2 4;
       Place E 1 0 3;
       Place F 0 4 0;
       Place G 0 1 2;
       Place H 6 4 4]);
   ((3, 23),
     [Place A 4 5 1;
       Place B 1 3 5;
       Place C 2 3 0;
       Place D 0 0 4;
       Place E 2 0 0;
       Place F 4 3 1;
       Place G 3 0 1;
       Place H 7 1 3]);
   ((3, 24),
     [Place A 2 2 0;
       Place B 0 0 3;
       Place C 0 2 3;
       Place D 0 2 1;
       Place E 2 4 0;
       Place F 7 4 3;
       Place G 0 3 4;
       Place H 2 0 0]);
   ((3, 25),
     [Place A 1 0 0;
       Place B 0 5 0;
       Place C 6 2 5;
       Place D 3 1 2;
       Place E 1 0 3;
       Place F 2 1 0;
       Place G 3 3 1;
       Place H 3 3 4]);
   ((3, 26),
     [Place A 6 2 3;
       Place B 1 3 5;
       Place C 2 0 0;
       Place D 0 0 4;
       Place E 2 4 0;
       Place F 7 0 1;
       Place G 0 2 1;
       Place H 7 3 1]);
   ((3, 27),
     [Place A 3 1 4;
       Place B 1 0 0;
       Place C 3 2 5;
       Place D 0 3 0;
       Place E 2 4 0;
       Place F 2 1 2;
       Place G 0 0 3;
       Place H 5 3 3]);
   ((3, 28),
     [Place A 1 0 0;
       Place B 1 4 1;
       Place C 2 3 0;
       Place D 1 1 0;
       Place E 1 2 4;
       Place F 3 2 3;
       Place G 2 3 4;
       Place H 1 0 3]);
   ((3, 29),
     [Place A 3 3 0;
       Place B 1 0 0;
       Place C 0 5 2;
       Place D 2 1 1;
       Place E 3 2 1;
       Place F 5 2 5;
       Place G 1 2 4;
       Place H 1 0 3]);
   ((3, 30),
     [Place A 5 3 0;
       Place B 0 4 4;
       Place C 2 0 0;
       Place D 1 1 4;
       Place E 0 3 3;
       Place F 6 3 1;
       Place G 0 0 3;
       Place H 2 1 1]);
   ((3, 31),
     [Place A 1 0 0;
       Place B 0 3 3;
       Place C 6 2 5;
       Place D 3 1 2;
       Place E 1 0 3;
       Place F 4 4 1;
       Place G 1 4 0;
       Place H 5 1 0]);
   ((4, 1),
     [Place A 0 0 0;
       Place B 0 5 0;
       Place C 4 3 0;
       Place D 0 1 0;
       Place E 3 0 2;
       Place F 3 0 5;
       Place G 2 3 3;
       Place H 7 3 5]);
   ((4, 2),
     [Place A 0 0 0;
       Place B 1 0 4;
       Place C 7 2 3;
       Place D 2 2 1;
       Place E 2 4 0;
       Place F 7 4 1;
       Place G 1 1 0;
       Place H 1 4 4]);
   ((4, 3),
     [Place A 0 0 0;
       Place B 1 1 0;
       Place C 6 2 5;
       Place D 2 0 3;
       Place E 3 1 3;
       Place F 6 3 1;
       Place G 1 4 0;
       Place H 0 4 3]);
   ((4, 4),
     [Place A 6 0 4;
       Place B 0 5 0;
       Place C 7 3 0;
       Place D 0 0 0;
       Place E 2 1 0;
       Place F 5 2 5;
       Place G 3 0 2;
       Place H 6 4 3]);
   ((4, 5),
     [Place A 5 0 3;
       Place B 1 1 1;
       Place C 4 4 3;
       Place D 1 3 0;
       Place E 0 0 0;
       Place F 3 0 5;
       Place G 3 3 4;
       Place H 2 5 0]);
   ((4, 6),
     [Place A 5 1 2;
       Place B 0 3 3;
       Place C 6 2 5;
       Place D 2 1 0;
       Place E 0 0 0;
       Place F 0 5 1;
       Place G 1 4 0;
       Place H 3 0 4]);
   ((4, 7),
     [Place A 7 0 2;
       Place B 0 5 0;
       Place C 4 3 0;
       Place D 3 1 3;
       Place E 0 0 0;
       Place F 7 4 3;
       Place G 0 3 4;
       Place H 5 1 1]);
   ((4, 8),
     [Place A 7 0 2;
       Place B 0 5 0;
       Place C 1 4 0;
       Place D 3 1 3;
       Place E 0 0 0;
       Place F 6 2 5;
       Place G 1 3 4;
       Place H 5 1 1]);
   ((4, 9),
     [Place A 7 0 2;
       Place B 0 5 0;
       Place C 6 2 5;
       Place D 1 1 1;
       Place E 0 0 0;
       Place F 5 1 4;
       Place G 3 3 0;
       Place H 7 3 3]);
   ((4, 10),
     [Place A 7 0 2;
       Place B 0 4 1;
       Place C 5 0 0;
       Place D 3 1 3;
       Place E 2 4 0;
       Place F 5 2 5;
       Place G 1 0 1;
       Place H 5 3 4]);
   ((4, 11),
     [Place A 7 0 2;
       Place B 0 4 3;
       Place C 5 0 0;
       Place D 3 3 0;
       Place E 1 3 4;
       Place F 7 1 3;
       Place G 1 0 1;
       Place H 6 5 0]);
   ((4, 12),
     [Place A 7 0 2;
       Place B 0 4 3;
       Place C 5 0 0;
       Place D 3 1 3;
       Place E 2 4 0;
       Place F 5 2 5;
       Place G 1 0 1;
       Place H 7 3 1]);
   ((4, 13),
     [Place A 6 0 4;
       Place B 1 3 1;
       Place C 2 0 0;
       Place D 1 1 1;
       Place E 2 4 0;
       Place F 6 2 5;
       Place G 3 0 2;
       Place H 2 4 3]);
   ((4, 14),
     [Place A 0 2 2;
       Place B 0 4 4;
       Place C 1 3 0;
       Place D 0 0 4;
       Place E 2 4 0;
       Place F 4 0 1;
       Place G 1 0 0;
       Place H 2 4 1]);
   ((4, 15),
     [Place A 6 3 0;
       Place B 0 4 3;
       Place C 6 3 1;
       Place D 0 0 4;
       Place E 1 3 4;
       Place F 2 0 0;
       Place G 2 0 1;
       Place H 3 1 3]);
   ((4, 16),
     [Place A 7 5 0;
       Place B 0 2 1;
       Place C 5 2 0;
       Place D 0 0 4;
       Place E 1 1 2;
       Place F 1 4 2;
       Place G 2 3 5;
       Place H 0 0 0]);
   ((4, 17),
     [Place A 3 3 1;
       Place B 0 4 3;
       Place C 5 3 0;
       Place D 0 0 4;
       Place E 1 3 4;
       Place F 4 0 1;
       Place G 1 0 0;
       Place H 0 2 2]);
   ((4, 18),
     [Place A 7 0 2;
       Place B 1 4 0;
       Place C 5 0 0;
       Place D 3 1 3;
       Place E 2 3 4;
       Place F 3 3 2;
       Place G 1 0 1;
       Place H 7 2 5]);
   ((4, 19),
     [Place A 0 3 0;
       Place B 1 3 5;
       Place C 4 4 1;
       Place D 0 0 4;
       Place E 2 4 0;
       Place F 4 0 1;
       Place G 1 0 0;
       Place H 1 2 2]);
   ((4, 20),
     [Place A 7 3 2;
       Place B 1 0 4;
       Place C 6 2 5;
       Place D 2 1 0;
       Place E 0 0 0;
       Place F 0 5 1;
       Place G 1 4 0;
       Place H 7 1 2]);
   ((4, 21),
     [Place A 3 0 5;
       Place B 0 3 1;
       Place C 7 4 1;
       Place D 0 3 4;
       Place E 2 4 0;
       Place F 2 0 0;
       Place G 2 0 1;
       Place H 7 0 3]);
   ((4, 22),
     [Place A 7 5 0;
       Place B 1 3 5;
       Place C 5 0 0;
       Place D 0 0 4;
       Place E 1 3 2;
       Place F 0 4 0;
       Place G 1 0 1;
       Place H 1 1 2]);
   ((4, 23),
     [Place A 4 5 1;
       Place B 1 3 5;
       Place C 2 3 0;
       Place D 0 0 4;
       Place E 0 0 0;
       Place F 4 3 1;
       Place G 2 1 3;
       Place H 2 1 1]);
   ((4, 24),
     [Place A 2 2 0;
       Place B 0 0 0;
       Place C 0 2 3;
       Place D 0 2 1;
       Place E 2 4 0;
       Place F 7 4 3;
       Place G 0 3 4;
       Place H 6 0 3]);
   ((4, 25),
     [Place A 7 0 2;
       Place B 0 5 0;
       Place C 4 3 0;
       Place D 3 1 3;
       Place E 0 0 0;
       Place F 6 2 5;
       Place G 1 3 4;
       Place H 5 1 1]);
   ((4, 26),
     [Place A 6 2 3;
       Place B 1 3 5;
       Place C 2 0 0;
       Place D 0 0 4;
       Place E 2 4 0;
       Place F 4 0 1;
       Place G 0 2 1;
       Place H 7 3 1]);
   ((4, 27),
     [Place A 5 1 2;
       Place B 0 3 3;
       Place C 3 2 5;
       Place D 2 1 0;
       Place E 0 0 0;
       Place F 0 5 1;
       Place G 1 4 0;
       Place H 3 0 4]);
   ((4, 28),
     [Place A 7 0 2;
       Place B 0 3 4;
       Place C 5 0 0;
       Place D 3 3 0;
       Place E 2 3 3;
       Place F 7 1 3;
       Place G 1 0 1;
       Place H 6 5 0]);
   ((4, 29),
     [Place A 7 0 2;
       Place B 0 3 3;
       Place C 5 0 0;
       Place D 3 3 0;
       Place E 3 3 4;
       Place F 7 1 3;
       Place G 1 0 1;
       Place H 0 5 1]);
   ((4, 30),
     [Place A 5 3 0;
       Place B 0 0 0;
       Place C 6 1 2;
       Place D 0 0 4;
       Place E 2 1 4;
       Place F 0 5 1;
       Place G 0 2 0;
       Place H 1 4 4]);
   ((5, 1),
     [Place A 5 0 4;
       Place B 1 2 5;
       Place C 0 0 0;
       Place D 0 2 1;
       Place E 1 1 1;
       Place F 4 4 3;
       Place G 3 5 0;
       Place H 4 3 0]);
   ((5, 2),
     [Place A 5 0 4;
       Place B 1 2 5;
       Place C 0 0 0;
       Place D 2 4 1;
       Place E 1 1 1;
       Place F 4 4 3;
       Place G 3 2 0;
       Place H 3 4 0]);
   ((5, 3),
     [Place A 5 0 4;
       Place B 1 2 5;
       Place C 1 0 0;
       Place D 2 4 1;
       Place E 0 1 0;
       Place F 4 4 3;
       Place G 3 2 1;
       Place H 3 4 0]);
   ((5, 4),
     [Place A 5 0 4;
       Place B 1 2 5;
       Place C 1 0 0;
       Place D 1 2 1;
       Place E 3 4 0;
       Place F 4 4 3;
       Place G 0 1 0;
       Place H 5 3 0]);
   ((5, 5),
     [Place A 3 0 5;
       Place B 1 3 0;
       Place C 7 4 3;
       Place D 2 1 2;
       Place E 3 4 0;
       Place F 0 0 0;
       Place G 3 1 0;
       Place H 6 3 3]);
   ((5, 6),
     [Place A 5 0 4;
       Place B 0 4 3;
       Place C 1 0 0;
       Place D 3 3 0;
       Place E 0 1 0;
       Place F 5 2 5;
       Place G 0 2 1;
       Place H 6 5 0]);
   ((5, 7),
     [Place A 6 0 4;
       Place B 1 3 0;
       Place C 4 4 3;
       Place D 2 1 2;
       Place E 3 4 0;
       Place F 0 0 0;
       Place G 3 1 0;
       Place H 6 3 4]);
   ((5, 8),
     [Place A 5 0 4;
       Place B 1 2 5;
       Place C 6 1 2;
       Place D 2 0 1;
       Place E 2 4 0;
       Place F 4 4 3;
       Place G 1 3 1;
       Place H 3 0 0]);
   ((5, 9),
     [Place A 5 0 4;
       Place B 1 2 5;
       Place C 0 0 0;
       Place D 2 4 1;
       Place E 1 1 1;
       Place F 4 4 3;
       Place G 0 2 0;
       Place H 3 4 0]);
   ((5, 10),
     [Place A 5 0 4;
       Place B 1 2 5;
       Place C 1 0 0;
       Place D 2 4 1;
       Place E 0 1 0;
       Place F 4 4 3;
       Place G 0 2 1;
       Place H 3 4 0]);
   ((5, 11),
     [Place A 5 0 4;
       Place B 1 2 5;
       Place C 1 0 0;
       Place D 0 3 0;
       Place E 2 4 0;
       Place F 4 4 3;
       Place G 1 2 2;
       Place H 0 1 0]);
   ((5, 12),
     [Place A 3 0 5;
       Place B 0 0 1;
       Place C 7 4 3;
       Place D 3 1 2;
       Place E 3 4 0;
       Place F 3 0 0;
       Place G 3 3 3;
       Place H 7 3 0]);
   ((5, 13),
     [Place A 5 0 4;
       Place B 0 4 3;
       Place C 3 2 5;
       Place D 3 0 1;
       Place E 0 0 0;
       Place F 3 3 0;
       Place G 2 4 1;
       Place H 6 2 1]);
   ((5, 14),
     [Place A 5 0 4;
       Place B 0 5 0;
       Place C 4 3 0;
       Place D 3 0 1;
       Place E 0 0 0;
       Place F 4 4 3;
       Place G 1 2 5;
       Place H 6 2 1]);
   ((5, 15),
     [Place A 5 0 4;
       Place B 0 5 0;
       Place C 2 0 0;
       Place D 2 2 4;
       Place E 3 3 4;
       Place F 5 2 2;
       Place G 1 2 1;
       Place H 6 0 1]);
   ((5, 16),
     [Place A 5 0 4;
       Place B 1 2 5;
       Place C 5 2 0;
       Place D 1 2 1;
       Place E 3 4 0;
       Place F 4 4 3;
       Place G 2 0 2;
       Place H 2 0 0]);
   ((5, 17),
     [Place A 5 0 4;
       Place B 1 2 5;
       Place C 2 2 0;
       Place D 1 0 0;
       Place E 1 0 1;
       Place F 4 4 3;
       Place G 0 3 1;
       Place H 6 5 0]);
   ((5, 18),
     [Place A 5 0 4;
       Place B 1 2 5;
       Place C 1 0 0;
       Place D 3 3 0;
       Place E 0 1 0;
       Place F 4 4 3;
       Place G 0 2 1;
       Place H 6 5 0]);
   ((5, 19),
     [Place A 5 0 4;
       Place B 1 2 5;
       Place C 4 4 3;
       Place D 3 0 1;
       Place E 0 0 0;
       Place F 3 3 0;
       Place G 2 4 1;
       Place H 6 2 1]);
   ((5, 20),
     [Place A 3 0 5;
       Place B 1 4 0;
       Place C 7 4 3;
       Place D 0 1 3;
       Place E 0 4 2;
       Place F 1 0 0;
       Place G 1 1 0;
       Place H 1 2 1]);
   ((5, 21),
     [Place A 0 4 3;
       Place B 0 2 4;
       Place C 4 4 1;
       Place D 0 0 1;
       Place E 2 4 0;
       Place F 1 3 0;
       Place G 3 0 3;
       Place H 5 0 0]);
   ((5, 22),
     [Place A 7 5 0;
       Place B 0 4 4;
       Place C 2 1 0;
       Place D 2 2 2;
       Place E 1 0 0;
       Place F 2 2 1;
       Place G 3 0 3;
       Place H 6 2 4]);
   ((5, 23),
     [Place A 4 5 1;
       Place B 1 3 5;
       Place C 2 3 0;
       Place D 3 0 3;
       Place E 0 2 4;
       Place F 0 0 0;
       Place G 3 1 0;
       Place H 6 3 1]);
   ((5, 24),
     [Place A 2 2 0;
       Place B 0 0 0;
       Place C 0 2 3;
       Place D 0 2 1;
       Place E 2 4 0;
       Place F 4 4 3;
       Place G 3 0 3;
       Place H 1 3 4]);
   ((5, 25),
     [Place A 5 0 4;
       Place B 1 4 0;
       Place C 2 0 0;
       Place D 2 2 4;
       Place E 3 3 4;
       Place F 2 3 2;
       Place G 0 2 1;
       Place H 6 0 1]);
   ((5, 26),
     [Place A 5 0 4;
       Place B 0 0 0;
       Place C 6 2 5;
       Place D 0 3 1;
       Place E 3 2 3;
       Place F 6 0 2;
       Place G 1 2 0;
       Place H 2 5 0]);
   ((5, 27),
     [Place A 2 0 0;
       Place B 0 5 0;
       Place C 3 2 5;
       Place D 0 0 1;
       Place E 2 2 0;
       Place F 7 2 2;
       Place G 3 0 3;
       Place H 0 4 3]);
   ((5, 28),
     [Place A 5 0 4;
       Place B 1 2 5;
       Place C 2 3 0;
       Place D 3 0 1;
       Place E 0 0 0;
       Place F 7 4 2;
       Place G 1 4 1;
       Place H 4 2 2]);
   ((5, 29),
     [Place A 5 0 4;
       Place B 1 2 5;
       Place C 2 2 0;
       Place D 2 4 1;
       Place E 0 3 1;
       Place F 4 4 3;
       Place G 2 0 2;
       Place H 2 0 0]);
   ((5, 30),
     [Place A 5 3 0;
       Place B 0 0 0;
       Place C 0 2 0;
       Place D 0 3 4;
       Place E 1 2 4;
       Place F 6 3 1;
       Place G 3 0 3;
       Place H 5 3 3]);
   ((5, 31),
     [Place A 5 0 4;
       Place B 0 0 0;
       Place C 3 2 5;
       Place D 2 4 0;
       Place E 3 0 1;
       Place F 2 2 0;
       Place G 2 3 2;
       Place H 7 3 4]);
   ((6, 1),
     [Place A 2 0 0;
       Place B 0 4 3;
       Place C 1 0 1;
       Place D 1 3 0;
       Place E 2 1 2;
       Place F 5 2 5;
       Place G 3 1 3;
       Place H 2 5 0]);
   ((6, 2),
     [Place A 3 2 0;
       Place B 0 0 0;
       Place C 4 4 3;
       Place D 2 0 2;
       Place E 3 2 4;
       Place F 0 3 1;
       Place G 3 5 0;
       Place H 4 1 4]);
   ((6, 3),
     [Place A 0 0 0;
       Place B 0 5 0;
       Place C 6 2 5;
       Place D 0 1 0;
       Place E 2 2 0;
       Place F 6 1 4;
       Place G 2 0 3;
       Place H 5 3 3]);
   ((6, 4),
     [Place A 7 5 0;
       Place B 0 1 0;
       Place C 1 0 0;
       Place D 3 3 1;
       Place E 0 3 0;
       Place F 2 0 4;
       Place G 3 4 4;
       Place H 3 2 5]);
   ((6, 5),
     [Place A 4 0 1;
       Place B 0 5 0;
       Place C 2 0 0;
       Place D 2 2 0;
       Place E 0 1 3;
       Place F 7 4 3;
       Place G 3 3 2;
       Place H 4 2 5]);
   ((6, 6),
     [Place A 0 0 2;
       Place B 0 4 4;
       Place C 7 2 3;
       Place D 1 2 0;
       Place E 2 4 0;
       Place F 7 1 1;
       Place G 3 4 1;
       Place H 2 0 0]);
   ((6, 7),
     [Place A 2 2 5;
       Place B 0 4 3;
       Place C 5 0 0;
       Place D 2 1 3;
       Place E 2 4 0;
       Place F 0 0 1;
       Place G 3 1 1;
       Place H 7 3 1]);
   ((6, 8),
     [Place A 5 3 0;
       Place B 1 0 0;
       Place C 6 2 5;
       Place D 1 3 2;
       Place E 0 0 2;
       Place F 5 1 4;
       Place G 1 1 3;
       Place H 0 5 1]);
   ((6, 9),
     [Place A 7 5 0;
       Place B 1 0 0;
       Place C 6 2 5;
       Place D 1 3 2;
       Place E 0 0 2;
       Place F 5 1 4;
       Place G 1 1 3;
       Place H 5 3 0]);
   ((6, 10),
     [Place A 6 3 0;
       Place B 0 0 0;
       Place C 0 2 3;
       Place D 2 4 1;
       Place E 0 2 0;
       Place F 7 4 3;
       Place G 0 3 4;
       Place H 2 0 3]);
   ((6, 11),
     [Place A 7 5 0;
       Place B 1 3 0;
       Place C 6 2 5;
       Place D 1 3 2;
       Place E 1 0 2;
       Place F 6 1 4;
       Place G 1 0 0;
       Place H 1 1 1]);
   ((6, 12),
     [Place A 5 1 0;
       Place B 1 3 5;
       Place C 0 0 0;
       Place D 3 0 2;
       Place E 0 3 1;
       Place F 7 1 3;
       Place G 3 5 0;
       Place H 1 4 2]);
   ((6, 13),
     [Place A 2 1 5;
       Place B 1 2 3;
       Place C 3 1 1;
       Place D 0 3 0;
       Place E 2 4 0;
       Place F 7 4 3;
       Place G 1 0 0;
       Place H 1 0 2]);
   ((6, 14),
     [Place A 4 2 3;
       Place B 1 4 0;
       Place C 5 0 0;
       Place D 3 4 2;
       Place E 0 0 1;
       Place F 7 0 2;
       Place G 1 2 2;
       Place H 5 3 5]);
   ((6, 15),
     [Place A 6 3 0;
       Place B 1 0 3;
       Place C 6 3 1;
       Place D 1 3 4;
       Place E 2 3 3;
       Place F 2 0 0;
       Place G 2 0 1;
       Place H 5 1 5]);
   ((6, 16),
     [Place A 7 5 0;
       Place B 1 0 0;
       Place C 6 2 5;
       Place D 1 3 2;
       Place E 0 0 2;
       Place F 6 1 4;
       Place G 1 3 0;
       Place H 3 1 3]);
   ((6, 17),
     [Place A 3 3 1;
       Place B 1 0 0;
       Place C 5 3 0;
       Place D 1 3 4;
       Place E 2 3 3;
       Place F 2 0 2;
       Place G 2 0 3;
       Place H 5 1 5]);
   ((6, 18),
     [Place A 4 2 3;
       Place B 1 3 5;
       Place C 0 0 1;
       Place D 3 4 2;
       Place E 2 0 0;
       Place F 0 1 2;
       Place G 0 3 0;
       Place H 7 4 0]);
   ((6, 19),
     [Place A 0 0 2;
       Place B 1 3 5;
       Place C 4 1 3;
       Place D 0 3 0;
       Place E 2 4 0;
       Place F 5 1 1;
       Place G 1 3 3;
       Place H 3 0 0]);
   ((6, 20),
     [Place A 7 2 0;
       Place B 1 0 0;
       Place C 3 2 5;
       Place D 3 3 1;
       Place E 2 4 0;
       Place F 7 4 2;
       Place G 1 1 4;
       Place H 0 0 2]);
   ((6, 21),
     [Place A 0 4 3;
       Place B 0 0 0;
       Place C 6 0 3;
       Place D 1 2 0;
       Place E 3 0 1;
       Place F 0 5 1;
       Place G 1 4 0;
       Place H 5 1 5]);
   ((6, 22),
     [Place A 7 5 0;
       Place B 1 0 0;
       Place C 0 4 1;
       Place D 3 2 0;
       Place E 3 3 4;
       Place F 2 1 5;
       Place G 0 0 2;
       Place H 5 1 3]);
   ((6, 23),
     [Place A 4 5 1;
       Place B 0 0 2;
       Place C 2 3 0;
       Place D 1 3 3;
       Place E 1 3 4;
       Place F 7 1 3;
       Place G 2 2 1;
       Place H 3 0 0]);
   ((6, 24),
     [Place A 2 2 0;
       Place B 0 0 0;
       Place C 2 0 3;
       Place D 0 2 1;
       Place E 2 4 0;
       Place F 7 4 3;
       Place G 0 3 4;
       Place H 2 1 4]);
   ((6, 25),
     [Place A 6 3 0;
       Place B 0 0 0;
       Place C 6 0 2;
       Place D 2 4 1;
       Place E 0 2 0;
       Place F 2 0 4;
       Place G 3 4 4;
       Place H 3 2 5]);
   ((6, 26),
     [Place A 7 5 0;
       Place B 1 3 5;
       Place C 0 0 1;
       Place D 0 1 2;
       Place E 2 0 0;
       Place F 4 3 1;
       Place G 1 3 0;
       Place H 2 1 4]);
   ((6, 27),
     [Place A 4 5 1;
       Place B 0 0 0;
       Place C 3 2 5;
       Place D 1 2 0;
       Place E 3 0 1;
       Place F 2 0 4;
       Place G 3 3 3;
       Place H 3 4 0]);
   ((6, 28),
     [Place A 7 5 0;
       Place B 1 2 5;
       Place C 5 2 4;
       Place D 3 3 1;
       Place E 0 3 0;
       Place F 4 1 0;
       Place G 0 0 0;
       Place H 2 0 3]);
   ((6, 29),
     [Place A 3 3 0;
       Place B 0 4 3;
       Place C 2 3 2;
       Place D 2 0 2;
       Place E 1 3 4;
       Place F 5 1 0;
       Place G 0 0 0;
       Place H 2 1 4]);
   ((6, 30),
     [Place A 0 1 3;
       Place B 1 0 0;
       Place C 2 3 0;
       Place D 0 2 3;
       Place E 0 0 2;
       Place F 0 5 1;
       Place G 2 3 5;
       Place H 6 3 1]);
   ((7, 1),
     [Place A 1 0 0;
       Place B 0 4 4;
       Place C 1 0 2;
       Place D 1 1 4;
       Place E 0 3 3;
       Place F 3 3 0;
       Place G 2 4 1;
       Place H 0 2 1]);
   ((7, 2),
     [Place A 1 0 0;
       Place B 0 4 1;
       Place C 1 0 2;
       Place D 0 3 3;
       Place E 2 4 0;
       Place F 4 1 3;
       Place G 3 2 0;
       Place H 4 3 5]);
   ((7, 3),
     [Place A 1 0 0;
       Place B 0 3 3;
       Place C 1 0 2;
       Place D 0 3 1;
       Place E 3 3 4;
       Place F 4 1 3;
       Place G 1 2 0;
       Place H 2 5 0]);
   ((7, 4),
     [Place A 1 0 0;
       Place B 0 4 4;
       Place C 1 0 2;
       Place D 1 1 4;
       Place E 0 3 3;
       Place F 6 3 1;
       Place G 1 4 0;
       Place H 0 2 0]);
   ((7, 5),
     [Place A 0 0 0;
       Place B 1 2 5;
       Place C 4 4 3;
       Place D 0 3 0;
       Place E 2 4 0;
       Place F 4 1 0;
       Place G 0 3 2;
       Place H 1 0 3]);
   ((7, 6),
     [Place A 3 2 4;
       Place B 0 5 0;
       Place C 3 0 0;
       Place D 0 3 2;
       Place E 2 2 0;
       Place F 5 2 5;
       Place G 1 0 2;
       Place H 6 0 3]);
   ((7, 7),
     [Place A 0 0 0;
       Place B 0 5 0;
       Place C 5 1 4;
       Place D 0 3 2;
       Place E 1 0 3;
       Place F 4 1 0;
       Place G 2 3 5;
       Place H 2 3 0]);
   ((7, 8),
     [Place A 2 1 5;
       Place B 1 3 1;
       Place C 1 0 0;
       Place D 2 0 3;
       Place E 2 4 0;
       Place F 4 4 3;
       Place G 0 3 3;
       Place H 6 1 0]);
   ((7, 9),
     [Place A 1 0 0;
       Place B 0 4 1;
       Place C 1 0 2;
       Place D 0 3 3;
       Place E 2 4 0;
       Place F 4 1 3;
       Place G 0 2 0;
       Place H 4 3 5]);
   ((7, 10),
     [Place A 1 0 0;
       Place B 0 5 0;
       Place C 1 0 2;
       Place D 2 3 3;
       Place E 0 2 0;
       Place F 4 1 3;
       Place G 3 3 1;
       Place H 7 3 5]);
   ((7, 11),
     [Place A 0 0 0;
       Place B 0 5 0;
       Place C 1 4 0;
       Place D 3 1 2;
       Place E 1 0 3;
       Place F 6 2 5;
       Place G 1 3 4;
       Place H 7 1 0]);
   ((7, 12),
     [Place A 0 0 0;
       Place B 1 3 0;
       Place C 6 2 5;
       Place D 0 3 2;
       Place E 3 4 0;
       Place F 4 1 0;
       Place G 2 2 4;
       Place H 1 0 3]);
   ((7, 13),
     [Place A 0 0 0;
       Place B 1 3 0;
       Place C 6 2 5;
       Place D 0 3 2;
       Place E 3 4 0;
       Place F 4 1 0;
       Place G 1 2 4;
       Place H 1 0 3]);
   ((7, 14),
     [Place A 1 0 0;
       Place B 1 4 0;
       Place C 0 5 2;
       Place D 1 3 4;
       Place E 1 0 2;
       Place F 3 0 5;
       Place G 2 2 2;
       Place H 2 2 0]);
   ((7, 15),
     [Place A 0 0 0;
       Place B 1 2 4;
       Place C 6 2 5;
       Place D 0 3 2;
       Place E 3 4 0;
       Place F 4 1 0;
       Place G 2 3 0;
       Place H 1 0 3]);
   ((7, 16),
     [Place A 0 0 0;
       Place B 1 2 4;
       Place C 6 2 5;
       Place D 0 3 2;
       Place E 3 4 0;
       Place F 4 1 0;
       Place G 1 3 0;
       Place H 1 0 3]);
   ((7, 17),
     [Place A 0 0 0;
       Place B 0 3 3;
       Place C 6 2 5;
       Place D 3 1 2;
       Place E 1 0 3;
       Place F 0 5 1;
       Place G 1 4 0;
       Place H 7 1 0]);
   ((7, 18),
     [Place A 1 0 0;
       Place B 1 3 0;
       Place C 1 0 2;
       Place D 1 1 4;
       Place E 3 4 0;
       Place F 1 2 0;
       Place G 2 3 3;
       Place H 5 3 5]);
   ((7, 19),
     [Place A 1 0 0;
       Place B 1 3 0;
       Place C 1 0 2;
       Place D 1 1 4;
       Place E 3 4 0;
       Place F 1 2 0;
       Place G 1 3 3;
       Place H 5 3 5]);
   ((7, 20),
     [Place A 0 0 0;
       Place B 0 5 0;
       Place C 1 4 0;
       Place D 1 2 2;
       Place E 3 3 4;
       Place F 3 0 5;
       Place G 2 0 3;
       Place H 7 1 0]);
   ((7, 21),
     [Place A 0 4 3;
       Place B 1 2 0;
       Place C 4 2 3;
       Place D 0 0 4;
       Place E 2 3 2;
       Place F 0 0 0;
       Place G 0 1 2;
       Place H 2 5 0]);
   ((7, 22),
     [Place A 7 5 0;
       Place B 1 3 5;
       Place C 1 0 0;
       Place D 0 0 4;
       Place E 1 3 2;
       Place F 0 4 0;
       Place G 3 1 2;
       Place H 7 1 0]);
   ((7, 23),
     [Place A 4 5 1;
       Place B 0 0 2;
       Place C 4 2 0;
       Place D 0 0 0;
       Place E 0 4 0;
       Place F 3 0 5;
       Place G 2 2 3;
       Place H 7 3 5]);
   ((7, 24),
     [Place A 1 0 0;
       Place B 0 2 0;
       Place C 1 0 2;
       Place D 0 3 3;
       Place E 2 4 0;
       Place F 4 1 3;
       Place G 0 4 1;
       Place H 4 3 5]);
   ((7, 25),
     [Place A 0 0 0;
       Place B 0 4 4;
       Place C 7 2 3;
       Place D 3 1 2;
       Place E 1 0 3;
       Place F 0 4 0;
       Place G 3 5 0;
       Place H 7 1 0]);
   ((7, 26),
     [Place A 1 0 0;
       Place B 0 5 0;
       Place C 1 0 2;
       Place D 1 1 4;
       Place E 0 3 3;
       Place F 0 2 0;
       Place G 3 3 0;
       Place H 1 4 4]);
   ((7, 27),
     [Place A 4 5 1;
       Place B 0 3 1;
       Place C 3 2 5;
       Place D 3 0 3;
       Place E 0 0 2;
       Place F 3 3 0;
       Place G 2 0 0;
       Place H 5 2 4]);
   ((7, 28),
     [Place A 1 0 0;
       Place B 0 5 0;
       Place C 1 0 2;
       Place D 1 1 4;
       Place E 2 3 3;
       Place F 0 2 0;
       Place G 3 3 0;
       Place H 2 3 4]);
   ((7, 29),
     [Place A 1 0 0;
       Place B 0 4 4;
       Place C 1 0 2;
       Place D 1 1 4;
       Place E 0 3 3;
       Place F 0 2 0;
       Place G 3 3 0;
       Place H 1 5 0]);
   ((7, 30),
     [Place A 1 0 0;
       Place B 0 4 4;
       Place C 1 0 2;
       Place D 1 1 4;
       Place E 0 3 3;
       Place F 1 2 0;
       Place G 0 5 0;
       Place H 2 3 0]);
   ((7, 31),
     [Place A 0 0 0;
       Place B 0 3 3;
       Place C 6 2 5;
       Place D 3 1 2;
       Place E 1 0 3;
       Place F 4 4 1;
       Place G 1 4 0;
       Place H 7 1 0]);
   ((8, 1),
     [Place A 7 0 2;
       Place B 0 4 1;
       Place C 0 0 0;
       Place D 3 1 3;
       Place E 2 4 0;
       Place F 6 2 5;
       Place G 1 3 4;
       Place H 6 2 0]);
   ((8, 2),
     [Place A 3 1 0;
       Place B 1 3 5;
       Place C 1 0 0;
       Place D 0 0 4;
       Place E 2 4 0;
       Place F 4 4 1;
       Place G 3 1 2;
       Place H 1 3 2]);
   ((8, 3),
     [Place A 3 0 0;
       Place B 0 5 0;
       Place C 3 2 5;
       Place D 3 0 3;
       Place E 3 3 3;
       Place F 0 0 1;
       Place G 3 3 0;
       Place H 7 2 3]);
   ((8, 4),
     [Place A 0 0 0;
       Place B 1 4 1;
       Place C 2 3 0;
       Place D 1 1 0;
       Place E 1 2 4;
       Place F 4 4 3;
       Place G 0 3 3;
       Place H 1 0 3]);
   ((8, 5),
     [Place A 0 0 0;
       Place B 1 3 0;
       Place C 3 2 5;
       Place D 1 1 0;
       Place E 3 4 0;
       Place F 3 2 3;
       Place G 2 3 4;
       Place H 1 0 3]);
   ((8, 6),
     [Place A 4 5 1;
       Place B 0 0 3;
       Place C 6 2 5;
       Place D 1 1 2;
       Place E 0 0 0;
       Place F 3 3 0;
       Place G 3 3 3;
       Place H 5 2 1]);
   ((8, 7),
     [Place A 0 0 0;
       Place B 0 5 0;
       Place C 7 4 3;
       Place D 3 1 2;
       Place E 1 0 3;
       Place F 2 1 0;
       Place G 3 3 1;
       Place H 0 3 4]);
   ((8, 8),
     [Place A 3 0 0;
       Place B 1 4 0;
       Place C 1 0 1;
       Place D 0 2 3;
       Place E 3 3 4;
       Place F 3 0 5;
       Place G 1 1 2;
       Place H 3 4 2]);
   ((8, 9),
     [Place A 0 0 0;
       Place B 1 3 5;
       Place C 4 1 0;
       Place D 0 3 2;
       Place E 0 2 4;
       Place F 3 3 0;
       Place G 2 4 1;
       Place H 1 0 3]);
   ((8, 10),
     [Place A 3 0 0;
       Place B 0 5 0;
       Place C 4 3 0;
       Place D 0 0 1;
       Place E 1 0 3;
       Place F 4 4 3;
       Place G 2 1 3;
       Place H 7 2 5]);
   ((8, 11),
     [Place A 0 0 0;
       Place B 0 5 0;
       Place C 1 4 0;
       Place D 3 1 2;
       Place E 1 0 3;
       Place F 6 2 5;
       Place G 1 3 4;
       Place H 5 1 0]);
   ((8, 12),
     [Place A 0 0 0;
       Place B 0 5 0;
       Place C 0 2 3;
       Place D 3 3 3;
       Place E 3 3 4;
       Place F 3 1 0;
       Place G 2 2 1;
       Place H 1 0 3]);
   ((8, 13),
     [Place A 0 0 0;
       Place B 0 4 4;
       Place C 3 2 2;
       Place D 2 3 0;
       Place E 3 4 0;
       Place F 2 1 0;
       Place G 0 2 4;
       Place H 1 0 3]);
   ((8, 14),
     [Place A 0 0 0;
       Place B 0 5 0;
       Place C 5 1 0;
       Place D 1 3 4;
       Place E 2 3 3;
       Place F 3 0 5;
       Place G 2 0 3;
       Place H 4 2 1]);
   ((8, 15),
     [Place A 0 0 0;
       Place B 1 2 5;
       Place C 1 2 1;
       Place D 3 3 1;
       Place E 2 1 0;
       Place F 4 4 3;
       Place G 3 5 0;
       Place H 1 0 3]);
   ((8, 16),
     [Place A 0 0 0;
       Place B 0 5 0;
       Place C 6 2 5;
       Place D 0 2 1;
       Place E 1 2 3;
       Place F 3 1 0;
       Place G 2 3 3;
       Place H 1 0 3]);
   ((8, 17),
     [Place A 0 0 0;
       Place B 0 3 3;
       Place C 6 2 5;
       Place D 3 1 2;
       Place E 1 0 3;
       Place F 0 5 1;
       Place G 1 4 0;
       Place H 5 1 0]);
   ((8, 18),
     [Place A 0 0 0;
       Place B 1 3 0;
       Place C 0 2 3;
       Place D 1 1 0;
       Place E 3 4 0;
       Place F 7 4 3;
       Place G 0 3 4;
       Place H 1 0 3]);
   ((8, 19),
     [Place A 0 0 0;
       Place B 1 4 1;
       Place C 2 3 0;
       Place D 1 1 0;
       Place E 1 0 3;
       Place F 5 2 5;
       Place G 3 4 3;
       Place H 7 1 3]);
   ((8, 20),
     [Place A 1 4 3;
       Place B 1 4 0;
       Place C 5 0 0;
       Place D 2 1 3;
       Place E 0 0 3;
       Place F 3 3 2;
       Place G 2 0 1;
       Place H 4 2 5]);
   ((8, 21),
     [Place A 0 4 3;
       Place B 0 0 3;
       Place C 4 4 1;
       Place D 1 2 0;
       Place E 2 4 0;
       Place F 1 2 1;
       Place G 0 0 0;
       Place H 6 2 4]);
   ((8, 22),
     [Place A 7 5 0;
       Place B 0 4 4;
       Place C 0 0 0;
       Place D 3 1 0;
       Place E 3 1 1;
       Place F 0 4 0;
       Place G 3 2 4;
       Place H 4 0 4]);
   ((8, 23),
     [Place A 4 5 1;
       Place B 1 3 5;
       Place C 6 1 3;
       Place D 0 0 4;
       Place E 0 4 0;
       Place F 6 0 2;
       Place G 0 0 0;
       Place H 2 2 0]);
   ((8, 24),
     [Place A 2 2 0;
       Place B 1 1 3;
       Place C 0 0 0;
       Place D 0 0 4;
       Place E 2 4 0;
       Place F 5 1 1;
       Place G 2 3 5;
       Place H 0 4 3]);
   ((8, 25),
     [Place A 0 0 0;
       Place B 0 5 0;
       Place C 6 2 5;
       Place D 3 1 2;
       Place E 1 0 3;
       Place F 2 1 0;
       Place G 3 3 1;
       Place H 3 3 4]);
   ((8, 26),
     [Place A 0 0 0;
       Place B 1 4 1;
       Place C 2 3 0;
       Place D 1 1 0;
       Place E 1 0 3;
       Place F 5 2 5;
       Place G 0 4 3;
       Place H 7 1 3]);
   ((8, 27),
     [Place A 4 5 1;
       Place B 0 0 3;
       Place C 3 2 5;
       Place D 1 1 2;
       Place E 0 0 0;
       Place F 3 3 0;
       Place G 3 3 3;
       Place H 5 2 1]);
   ((8, 28),
     [Place A 0 0 0;
       Place B 1 4 1;
       Place C 2 3 0;
       Place D 1 1 0;
       Place E 1 2 4;
       Place F 3 2 3;
       Place G 2 3 4;
       Place H 1 0 3]);
   ((8, 29),
     [Place A 3 0 0;
       Place B 1 2 5;
       Place C 4 3 0;
       Place D 0 0 1;
       Place E 3 1 2;
       Place F 4 4 3;
       Place G 0 0 3;
       Place H 1 5 0]);
   ((8, 30),
     [Place A 5 3 0;
       Place B 0 0 3;
       Place C 0 2 0;
       Place D 0 3 4;
       Place E 1 2 4;
       Place F 6 3 1;
       Place G 0 0 0;
       Place H 5 3 3]);
   ((8, 31),
     [Place A 0 0 0;
       Place B 0 3 3;
       Place C 6 2 5;
       Place D 3 1 2;
       Place E 1 0 3;
       Place F 4 4 1;
       Place G 1 4 0;
       Place H 5 1 0]);
   ((9, 1),
     [Place A 2 0 0;
       Place B 1 0 4;
       Place C 5 3 0;
       Place D 1 2 2;
       Place E 1 0 1;
       Place F 5 2 5;
       Place G 3 4 3;
       Place H 4 4 1]);
   ((9, 2),
     [Place A 2 1 5;
       Place B 0 5 0;
       Place C 1 0 0;
       Place D 2 0 3;
       Place E 3 2 0;
       Place F 7 4 3;
       Place G 1 1 0;
       Place H 0 3 3]);
   ((9, 3),
     [Place A 3 0 0;
       Place B 0 5 0;
       Place C 0 0 1;
       Place D 3 0 3;
       Place E 1 2 4;
       Place F 7 4 3;
       Place G 3 3 0;
       Place H 0 3 3]);
   ((9, 4),
     [Place A 1 1 3;
       Place B 1 0 0;
       Place C 1 0 2;
       Place D 0 3 0;
       Place E 2 4 0;
       Place F 7 4 3;
       Place G 0 3 4;
       Place H 5 2 2]);
   ((9, 5),
     [Place A 6 1 0;
       Place B 0 0 3;
       Place C 3 2 5;
       Place D 0 2 2;
       Place E 0 0 0;
       Place F 3 3 2;
       Place G 2 3 4;
       Place H 7 4 0]);
   ((9, 6),
     [Place A 7 1 2;
       Place B 0 4 4;
       Place C 7 2 3;
       Place D 1 2 0;
       Place E 2 4 0;
       Place F 0 0 2;
       Place G 3 4 1;
       Place H 4 0 0]);
   ((9, 7),
     [Place A 2 0 0;
       Place B 0 5 0;
       Place C 5 0 5;
       Place D 3 1 2;
       Place E 2 2 0;
       Place F 1 0 1;
       Place G 1 3 3;
       Place H 1 4 4]);
   ((9, 8),
     [Place A 3 0 0;
       Place B 1 2 5;
       Place C 0 0 1;
       Place D 3 0 3;
       Place E 2 4 0;
       Place F 4 4 3;
       Place G 3 2 2;
       Place H 0 4 1]);
   ((9, 9),
     [Place A 2 2 2;
       Place B 1 0 0;
       Place C 1 0 2;
       Place D 2 2 4;
       Place E 3 3 4;
       Place F 3 3 0;
       Place G 2 4 1;
       Place H 4 1 3]);
   ((9, 10),
     [Place A 3 0 0;
       Place B 0 4 4;
       Place C 3 2 2;
       Place D 0 0 4;
       Place E 2 1 4;
       Place F 2 3 0;
       Place G 0 0 1;
       Place H 7 4 1]);
   ((9, 11),
     [Place A 1 1 3;
       Place B 1 0 0;
       Place C 1 0 2;
       Place D 0 3 0;
       Place E 2 4 0;
       Place F 4 4 3;
       Place G 1 2 2;
       Place H 1 3 4]);
   ((9, 12),
     [Place A 1 1 3;
       Place B 1 0 0;
       Place C 1 0 2;
       Place D 3 3 3;
       Place E 3 3 4;
       Place F 3 3 0;
       Place G 2 4 1;
       Place H 6 2 1]);
   ((9, 13),
     [Place A 3 0 5;
       Place B 1 0 0;
       Place C 7 4 3;
       Place D 0 3 0;
       Place E 2 4 0;
       Place F 4 3 2;
       Place G 0 2 2;
       Place H 1 0 2]);
   ((9, 14),
     [Place A 1 3 3;
       Place B 0 5 0;
       Place C 4 4 3;
       Place D 3 1 1;
       Place E 1 0 2;
       Place F 3 0 5;
       Place G 3 3 0;
       Place H 3 0 0]);
   ((9, 15),
     [Place A 6 3 0;
       Place B 1 2 5;
       Place C 0 2 0;
       Place D 2 4 1;
       Place E 3 1 2;
       Place F 4 4 3;
       Place G 0 0 3;
       Place H 0 0 0]);
   ((9, 16),
     [Place A 7 5 0;
       Place B 0 3 2;
       Place C 1 0 0;
       Place D 2 0 3;
       Place E 3 3 4;
       Place F 2 1 5;
       Place G 1 3 0;
       Place H 2 1 0]);
   ((9, 17),
     [Place A 3 3 1;
       Place B 1 0 0;
       Place C 5 3 0;
       Place D 1 2 2;
       Place E 3 0 3;
       Place F 5 2 5;
       Place G 3 4 3;
       Place H 1 0 2]);
   ((9, 18),
     [Place A 3 0 0;
       Place B 1 2 5;
       Place C 0 0 1;
       Place D 3 0 3;
       Place E 3 4 0;
       Place F 4 4 3;
       Place G 3 2 2;
       Place H 5 3 0]);
   ((9, 19),
     [Place A 0 4 1;
       Place B 1 3 5;
       Place C 0 0 0;
       Place D 0 0 4;
       Place E 0 3 0;
       Place F 4 1 0;
       Place G 2 1 3;
       Place H 6 5 0]);
   ((9, 20),
     [Place A 1 4 3;
       Place B 1 4 0;
       Place C 5 0 0;
       Place D 2 1 3;
       Place E 0 0 3;
       Place F 3 3 2;
       Place G 1 0 1;
       Place H 4 2 5]);
   ((9, 21),
     [Place A 0 4 3;
       Place B 1 0 0;
       Place C 5 3 0;
       Place D 0 0 4;
       Place E 2 1 4;
       Place F 0 5 1;
       Place G 2 0 2;
       Place H 0 3 1]);
   ((9, 22),
     [Place A 7 5 0;
       Place B 0 4 4;
       Place C 1 0 0;
       Place D 0 0 4;
       Place E 2 1 4;
       Place F 0 4 0;
       Place G 1 1 0;
       Place H 1 2 1]);
   ((9, 23),
     [Place A 4 5 1;
       Place B 1 3 5;
       Place C 2 3 0;
       Place D 0 0 4;
       Place E 2 0 0;
       Place F 4 3 1;
       Place G 0 0 1;
       Place H 7 1 3]);
   ((9, 24),
     [Place A 2 2 0;
       Place B 0 4 3;
       Place C 0 0 0;
       Place D 3 2 2;
       Place E 2 4 0;
       Place F 5 2 5;
       Place G 3 1 1;
       Place H 4 0 4]);
   ((9, 25),
     [Place A 1 1 3;
       Place B 1 0 0;
       Place C 1 0 2;
       Place D 0 3 0;
       Place E 2 4 0;
       Place F 1 4 2;
       Place G 2 3 5;
       Place H 2 2 2]);
   ((9, 26),
     [Place A 0 4 3;
       Place B 1 4 0;
       Place C 5 0 0;
       Place D 2 1 3;
       Place E 0 0 3;
       Place F 3 3 2;
       Place G 1 0 1;
       Place H 4 2 5]);
   ((9, 27),
     [Place A 0 3 0;
       Place B 0 0 3;
       Place C 3 2 5;
       Place D 1 1 1;
       Place E 0 0 0;
       Place F 0 5 1;
       Place G 1 4 0;
       Place H 5 2 4]);
   ((9, 28),
     [Place A 5 1 2;
       Place B 1 0 0;
       Place C 1 0 2;
       Place D 0 3 0;
       Place E 2 4 0;
       Place F 6 1 3;
       Place G 3 4 3;
       Place H 4 2 5]);
   ((9, 29),
     [Place A 1 1 3;
       Place B 1 0 0;
       Place C 1 0 2;
       Place D 3 2 0;
       Place E 3 2 1;
       Place F 7 4 3;
       Place G 0 3 4;
       Place H 1 5 0]);
   ((9, 30),
     [Place A 5 3 0;
       Place B 0 4 4;
       Place C 0 2 0;
       Place D 1 1 4;
       Place E 0 3 3;
       Place F 6 3 1;
       Place G 0 0 3;
       Place H 0 0 0]);
   ((10, 1),
     [Place A 2 0 0;
       Place B 0 5 0;
       Place C 4 4 3;
       Place D 0 0 1;
       Place E 1 0 3;
       Place F 6 1 3;
       Place G 3 3 0;
       Place H 7 2 5]);
   ((10, 2),
     [Place A 7 5 0;
       Place B 0 0 0;
       Place C 5 2 0;
       Place D 3 1 2;
       Place E 1 0 3;
       Place F 5 2 5;
       Place G 3 3 1;
       Place H 5 3 4]);
   ((10, 3),
     [Place A 0 3 2;
       Place B 0 0 0;
       Place C 2 2 0;
       Place D 1 3 1;
       Place E 3 3 4;
       Place F 3 0 5;
       Place G 2 0 3;
       Place H 6 5 0]);
   ((10, 4),
     [Place A 6 0 4;
       Place B 0 5 0;
       Place C 7 3 0;
       Place D 0 0 0;
       Place E 2 1 0;
       Place F 5 2 5;
       Place G 0 0 2;
       Place H 6 4 3]);
   ((10, 5),
     [Place A 3 0 5;
       Place B 0 4 1;
       Place C 1 0 1;
       Place D 1 2 3;
       Place E 2 4 0;
       Place F 2 0 0;
       Place G 3 4 4;
       Place H 7 1 1]);
   ((10, 6),
     [Place A 7 1 2;
       Place B 1 0 0;
       Place C 0 0 2;
       Place D 3 2 4;
       Place E 1 3 1;
       Place F 3 3 0;
       Place G 2 4 1;
       Place H 6 4 4]);
   ((10, 7),
     [Place A 6 0 4;
       Place B 1 3 5;
       Place C 1 0 1;
       Place D 3 4 2;
       Place E 3 2 1;
       Place F 3 0 0;
       Place G 2 1 1;
       Place H 5 4 0]);
   ((10, 8),
     [Place A 3 0 0;
       Place B 1 4 0;
       Place C 1 0 1;
       Place D 0 1 1;
       Place E 3 0 3;
       Place F 6 2 5;
       Place G 1 3 4;
       Place H 3 4 2]);
   ((10, 9),
     [Place A 4 2 3;
       Place B 0 0 0;
       Place C 0 2 0;
       Place D 3 3 3;
       Place E 3 3 4;
       Place F 6 3 1;
       Place G 1 4 0;
       Place H 1 0 3]);
   ((10, 10),
     [Place A 5 0 4;
       Place B 0 4 1;
       Place C 5 0 0;
       Place D 1 1 1;
       Place E 2 4 0;
       Place F 0 0 1;
       Place G 3 4 4;
       Place H 3 2 5]);
   ((10, 11),
     [Place A 6 0 4;
       Place B 0 4 3;
       Place C 5 0 0;
       Place D 3 3 0;
       Place E 2 0 1;
       Place F 5 2 5;
       Place G 0 0 2;
       Place H 6 5 0]);
   ((10, 12),
     [Place A 3 0 5;
       Place B 1 3 0;
       Place C 7 4 3;
       Place D 3 1 2;
       Place E 3 4 0;
       Place F 0 0 1;
       Place G 3 3 3;
       Place H 5 0 0]);
   ((10, 13),
     [Place A 3 0 5;
       Place B 0 4 4;
       Place C 0 0 1;
       Place D 2 3 2;
       Place E 2 4 0;
       Place F 3 0 0;
       Place G 3 1 2;
       Place H 3 3 1]);
   ((10, 14),
     [Place A 3 1 4;
       Place B 1 3 0;
       Place C 0 0 0;
       Place D 0 0 4;
       Place E 3 4 0;
       Place F 5 2 2;
       Place G 3 4 4;
       Place H 6 1 0]);
   ((10, 15),
     [Place A 7 5 0;
       Place B 1 0 0;
       Place C 0 0 2;
       Place D 3 1 2;
       Place E 3 3 4;
       Place F 2 1 5;
       Place G 2 3 0;
       Place H 6 3 2]);
   ((10, 16),
     [Place A 7 5 0;
       Place B 1 0 4;
       Place C 5 2 0;
       Place D 0 1 0;
       Place E 2 3 4;
       Place F 1 0 0;
       Place G 2 2 2;
       Place H 7 2 5]);
   ((10, 17),
     [Place A 3 3 1;
       Place B 1 0 0;
       Place C 5 3 0;
       Place D 0 3 2;
       Place E 1 2 2;
       Place F 3 0 5;
       Place G 0 0 2;
       Place H 7 3 5]);
   ((10, 18),
     [Place A 6 0 4;
       Place B 0 5 0;
       Place C 1 0 1;
       Place D 1 0 0;
       Place E 2 2 0;
       Place F 5 2 5;
       Place G 3 2 1;
       Place H 6 4 3]);
   ((10, 19),
     [Place A 4 3 1;
       Place B 1 3 5;
       Place C 0 0 0;
       Place D 0 0 4;
       Place E 2 4 0;
       Place F 7 4 1;
       Place G 3 1 2;
       Place H 7 1 0]);
   ((10, 20),
     [Place A 1 4 2;
       Place B 1 0 0;
       Place C 3 2 5;
       Place D 0 3 0;
       Place E 2 4 0;
       Place F 1 3 2;
       Place G 1 0 2;
       Place H 3 0 4]);
   ((10, 21),
     [Place A 0 4 3;
       Place B 1 0 0;
       Place C 5 3 0;
       Place D 0 0 4;
       Place E 2 1 4;
       Place F 0 5 1;
       Place G 1 0 2;
       Place H 0 3 1]);
   ((10, 22),
     [Place A 7 5 0;
       Place B 1 0 4;
       Place C 2 0 0;
       Place D 3 2 4;
       Place E 2 1 1;
       Place F 0 4 0;
       Place G 1 0 2;
       Place H 6 4 4]);
   ((10, 23),
     [Place A 4 5 1;
       Place B 1 3 5;
       Place C 7 1 1;
       Place D 0 0 4;
       Place E 0 4 0;
       Place F 1 0 0;
       Place G 1 1 0;
       Place H 1 3 2]);
   ((10, 24),
     [Place A 2 2 0;
       Place B 0 0 0;
       Place C 0 2 3;
       Place D 0 2 1;
       Place E 2 4 0;
       Place F 7 4 3;
       Place G 0 3 4;
       Place H 1 0 3]);
   ((10, 25),
     [Place A 6 0 4;
       Place B 0 5 0;
       Place C 1 0 1;
       Place D 1 0 0;
       Place E 2 2 0;
       Place F 5 2 5;
       Place G 3 2 1;
       Place H 1 4 3]);
   ((10, 26),
     [Place A 7 5 0;
       Place B 1 3 5;
       Place C 0 4 1;
       Place D 0 0 4;
       Place E 0 3 0;
       Place F 0 0 0;
       Place G 3 1 0;
       Place H 7 1 3]);
   ((10, 27),
     [Place A 4 5 1;
       Place B 1 0 0;
       Place C 1 4 3;
       Place D 0 0 4;
       Place E 2 1 4;
       Place F 3 3 0;
       Place G 1 0 2;
       Place H 0 3 1]);
   ((10, 28),
     [Place A 3 0 5;
       Place B 1 0 0;
       Place C 0 5 2;
       Place D 2 4 0;
       Place E 0 3 0;
       Place F 7 3 3;
       Place G 0 0 2;
       Place H 1 2 2]);
   ((10, 29),
     [Place A 6 0 4;
       Place B 0 4 3;
       Place C 1 0 1;
       Place D 1 0 0;
       Place E 2 2 0;
       Place F 5 2 5;
       Place G 3 2 1;
       Place H 1 5 0]);
   ((10, 30),
     [Place A 2 2 2;
       Place B 1 0 0;
       Place C 2 3 0;
       Place D 0 2 3;
       Place E 3 3 4;
       Place F 3 0 5;
       Place G 0 0 2;
       Place H 4 4 1]);
   ((10, 31),
     [Place A 4 5 0;
       Place B 0 0 0;
       Place C 2 2 0;
       Place D 3 1 2;
       Place E 1 0 3;
       Place F 5 2 5;
       Place G 3 3 1;
       Place H 5 3 4]);
   ((11, 1),
     [Place A 6 0 4;
       Place B 0 4 3;
       Place C 2 0 3;
       Place D 3 3 0;
       Place E 1 0 0;
       Place F 5 2 5;
       Place G 2 1 0;
       Place H 6 5 0]);
   ((11, 2),
     [Place A 6 0 4;
       Place B 0 4 3;
       Place C 2 0 3;
       Place D 1 3 0;
       Place E 0 0 0;
       Place F 5 2 5;
       Place G 2 1 1;
       Place H 2 5 0]);
   ((11, 3),
     [Place A 6 0 4;
       Place B 0 4 3;
       Place C 2 0 3;
       Place D 1 3 0;
       Place E 0 0 0;
       Place F 5 2 5;
       Place G 1 1 1;
       Place H 2 5 0]);
   ((11, 4),
     [Place A 6 0 4;
       Place B 1 2 0;
       Place C 6 2 5;
       Place D 1 0 0;
       Place E 2 3 2;
       Place F 1 0 1;
       Place G 3 3 3;
       Place H 2 5 0]);
   ((11, 5),
     [Place A 3 0 5;
       Place B 0 3 3;
       Place C 7 4 3;
       Place D 2 0 2;
       Place E 0 0 0;
       Place F 2 3 0;
       Place G 1 1 1;
       Place H 7 4 1]);
   ((11, 6),
     [Place A 0 1 1;
       Place B 0 4 1;
       Place C 1 0 2;
       Place D 3 2 4;
       Place E 2 4 0;
       Place F 7 2 0;
       Place G 1 0 0;
       Place H 6 4 4]);
   ((11, 7),
     [Place A 4 0 2;
       Place B 0 5 0;
       Place C 4 3 0;
       Place D 3 1 3;
       Place E 0 0 0;
       Place F 7 4 3;
       Place G 0 3 4;
       Place H 5 1 1]);
   ((11, 8),
     [Place A 4 0 2;
       Place B 0 5 0;
       Place C 1 4 0;
       Place D 3 1 3;
       Place E 0 0 0;
       Place F 6 2 5;
       Place G 1 3 4;
       Place H 5 1 1]);
   ((11, 9),
     [Place A 4 0 2;
       Place B 0 5 0;
       Place C 6 2 5;
       Place D 1 1 1;
       Place E 0 0 0;
       Place F 5 1 4;
       Place G 3 3 0;
       Place H 7 3 3]);
   ((11, 10),
     [Place A 4 0 2;
       Place B 0 4 1;
       Place C 5 0 0;
       Place D 3 1 3;
       Place E 2 4 0;
       Place F 5 2 5;
       Place G 1 0 1;
       Place H 5 3 4]);
   ((11, 11),
     [Place A 4 0 2;
       Place B 0 4 3;
       Place C 5 0 0;
       Place D 3 3 0;
       Place E 1 3 4;
       Place F 7 1 3;
       Place G 1 0 1;
       Place H 6 5 0]);
   ((11, 12),
     [Place A 4 0 2;
       Place B 0 4 3;
       Place C 5 0 0;
       Place D 3 1 3;
       Place E 2 4 0;
       Place F 5 2 5;
       Place G 1 0 1;
       Place H 7 3 1]);
   ((11, 13),
     [Place A 6 0 4;
       Place B 0 5 0;
       Place C 6 2 5;
       Place D 1 0 0;
       Place E 2 2 0;
       Place F 1 0 1;
       Place G 3 2 1;
       Place H 0 4 3]);
   ((11, 14),
     [Place A 4 0 2;
       Place B 0 3 0;
       Place C 4 1 1;
       Place D 2 3 2;
       Place E 0 0 0;
       Place F 2 1 5;
       Place G 3 4 4;
       Place H 2 5 0]);
   ((11, 15),
     [Place A 7 5 0;
       Place B 0 0 0;
       Place C 0 2 0;
       Place D 2 3 0;
       Place E 3 3 4;
       Place F 4 3 2;
       Place G 0 0 3;
       Place H 0 2 4]);
   ((11, 16),
     [Place A 7 5 0;
       Place B 0 0 0;
       Place C 5 2 0;
       Place D 0 0 4;
       Place E 3 0 1;
       Place F 3 2 4;
       Place G 2 3 5;
       Place H 1 3 1]);
   ((11, 17),
     [Place A 6 0 4;
       Place B 0 4 3;
       Place C 2 0 3;
       Place D 3 1 0;
       Place E 2 4 0;
       Place F 5 2 5;
       Place G 1 3 1;
       Place H 0 0 0]);
   ((11, 18),
     [Place A 4 0 2;
       Place B 1 4 0;
       Place C 5 0 0;
       Place D 3 1 3;
       Place E 2 3 4;
       Place F 3 3 2;
       Place G 1 0 1;
       Place H 7 2 5]);
   ((11, 19),
     [Place A 6 0 4;
       Place B 1 1 1;
       Place C 2 0 3;
       Place D 1 3 0;
       Place E 0 0 0;
       Place F 5 2 5;
       Place G 3 4 3;
       Place H 2 5 0]);
   ((11, 20),
     [Place A 0 2 2;
       Place B 0 0 0;
       Place C 3 2 5;
       Place D 3 3 1;
       Place E 2 4 0;
       Place F 7 4 2;
       Place G 0 0 3;
       Place H 2 2 0]);
   ((11, 21),
     [Place A 0 4 3;
       Place B 0 2 4;
       Place C 4 4 1;
       Place D 0 0 1;
       Place E 2 4 0;
       Place F 1 3 0;
       Place G 0 0 3;
       Place H 5 0 0]);
   ((11, 22),
     [Place A 7 5 0;
       Place B 0 4 4;
       Place C 2 1 0;
       Place D 2 2 2;
       Place E 1 0 0;
       Place F 2 2 1;
       Place G 0 0 3;
       Place H 6 2 4]);
   ((11, 23),
     [Place A 4 5 1;
       Place B 1 3 5;
       Place C 1 0 0;
       Place D 0 0 4;
       Place E 0 4 0;
       Place F 7 2 0;
       Place G 2 2 3;
       Place H 0 1 0]);
   ((11, 24),
     [Place A 2 2 0;
       Place B 0 1 1;
       Place C 0 0 0;
       Place D 0 0 4;
       Place E 2 4 0;
       Place F 3 2 4;
       Place G 2 3 5;
       Place H 4 3 2]);
   ((11, 25),
     [Place A 4 0 2;
       Place B 0 5 0;
       Place C 4 3 0;
       Place D 3 1 3;
       Place E 0 0 0;
       Place F 6 2 5;
       Place G 1 3 4;
       Place H 5 1 1]);
   ((11, 26),
     [Place A 6 0 4;
       Place B 1 1 1;
       Place C 2 0 3;
       Place D 1 3 0;
       Place E 0 0 0;
       Place F 5 2 5;
       Place G 0 4 3;
       Place H 2 5 0]);
   ((11, 27),
     [Place A 2 0 0;
       Place B 0 5 0;
       Place C 3 2 5;
       Place D 0 0 1;
       Place E 2 2 0;
       Place F 7 2 2;
       Place G 0 0 3;
       Place H 0 4 3]);
   ((11, 28),
     [Place A 4 0 2;
       Place B 0 3 4;
       Place C 5 0 0;
       Place D 3 3 0;
       Place E 2 3 3;
       Place F 7 1 3;
       Place G 1 0 1;
       Place H 6 5 0]);
   ((11, 29),
     [Place A 4 0 2;
       Place B 0 3 3;
       Place C 5 0 0;
       Place D 3 3 0;
       Place E 3 3 4;
       Place F 7 1 3;
       Place G 1 0 1;
       Place H 0 5 1]);
   ((11, 30),
     [Place A 5 3 0;
       Place B 0 0 0;
       Place C 0 2 0;
       Place D 0 3 4;
       Place E 1 2 4;
       Place F 6 3 1;
       Place G 0 0 3;
       Place H 5 3 3]);
   ((12, 1),
     [Place A 1 1 3;
       Place B 0 5 0;
       Place C 0 0 2;
       Place D 3 2 0;
       Place E 3 2 1;
       Place F 7 4 3;
       Place G 0 3 4;
       Place H 4 0 0]);
   ((12, 2),
     [Place A 4 0 2;
       Place B 0 4 4;
       Place C 7 2 3;
       Place D 1 2 0;
       Place E 2 4 0;
       Place F 7 1 2;
       Place G 3 4 1;
       Place H 0 0 0]);
   ((12, 3),
     [Place A 7 0 2;
       Place B 0 5 0;
       Place C 5 1 0;
       Place D 0 3 4;
       Place E 1 2 4;
       Place F 0 0 0;
       Place G 3 2 1;
       Place H 1 4 2]);
   ((12, 4),
     [Place A 4 0 2;
       Place B 0 3 3;
       Place C 7 4 3;
       Place D 1 1 4;
       Place E 0 0 0;
       Place F 3 3 0;
       Place G 2 4 1;
       Place H 5 1 1]);
   ((12, 5),
     [Place A 7 0 2;
       Place B 1 3 0;
       Place C 6 2 5;
       Place D 1 1 0;
       Place E 3 4 0;
       Place F 0 0 0;
       Place G 3 2 3;
       Place H 0 4 3]);
   ((12, 6),
     [Place A 7 0 2;
       Place B 0 4 4;
       Place C 0 0 0;
       Place D 0 3 0;
       Place E 2 4 0;
       Place F 4 1 0;
       Place G 3 2 4;
       Place H 4 3 2]);
   ((12, 7),
     [Place A 4 0 2;
       Place B 0 4 4;
       Place C 7 1 1;
       Place D 1 2 0;
       Place E 2 4 0;
       Place F 7 2 3;
       Place G 3 4 1;
       Place H 0 0 0]);
   ((12, 8),
     [Place A 4 0 2;
       Place B 0 5 0;
       Place C 3 2 5;
       Place D 1 1 1;
       Place E 0 0 0;
       Place F 3 1 4;
       Place G 3 4 3;
       Place H 6 3 0]);
   ((12, 9),
     [Place A 4 0 2;
       Place B 0 5 0;
       Place C 6 1 3;
       Place D 1 1 1;
       Place E 0 0 0;
       Place F 7 4 3;
       Place G 3 3 0;
       Place H 4 2 5]);
   ((12, 10),
     [Place A 4 0 2;
       Place B 0 5 0;
       Place C 3 2 5;
       Place D 1 1 1;
       Place E 0 0 0;
       Place F 3 1 4;
       Place G 3 4 3;
       Place H 2 3 0]);
   ((12, 11),
     [Place A 4 0 2;
       Place B 0 4 3;
       Place C 5 0 0;
       Place D 3 3 0;
       Place E 1 3 4;
       Place F 4 1 3;
       Place G 1 0 1;
       Place H 6 5 0]);
   ((12, 12),
     [Place A 4 0 2;
       Place B 1 3 0;
       Place C 7 4 3;
       Place D 1 1 4;
       Place E 3 4 0;
       Place F 1 2 0;
       Place G 3 3 3;
       Place H 0 0 0]);
   ((12, 13),
     [Place A 4 0 2;
       Place B 0 4 4;
       Place C 5 0 0;
       Place D 1 1 4;
       Place E 2 4 0;
       Place F 2 2 3;
       Place G 1 0 1;
       Place H 7 3 1]);
   ((12, 14),
     [Place A 4 0 2;
       Place B 0 4 4;
       Place C 5 0 0;
       Place D 3 3 0;
       Place E 0 3 3;
       Place F 4 1 3;
       Place G 1 0 1;
       Place H 6 5 0]);
   ((12, 15),
     [Place A 4 0 2;
       Place B 0 5 0;
       Place C 4 4 3;
       Place D 1 1 1;
       Place E 0 0 0;
       Place F 3 1 4;
       Place G 2 2 5;
       Place H 1 3 0]);
   ((12, 16),
     [Place A 4 0 2;
       Place B 0 5 0;
       Place C 6 1 3;
       Place D 1 1 1;
       Place E 0 0 0;
       Place F 7 4 3;
       Place G 0 3 0;
       Place H 4 2 5]);
   ((12, 17),
     [Place A 4 0 2;
       Place B 0 5 0;
       Place C 4 4 3;
       Place D 1 1 1;
       Place E 0 0 0;
       Place F 3 1 4;
       Place G 2 2 5;
       Place H 0 3 0]);
   ((12, 18),
     [Place A 4 0 2;
       Place B 0 4 4;
       Place C 5 0 0;
       Place D 1 1 4;
       Place E 2 4 0;
       Place F 4 2 2;
       Place G 1 0 1;
       Place H 2 4 1]);
   ((12, 19),
     [Place A 4 0 2;
       Place B 0 4 1;
       Place C 5 0 0;
       Place D 1 1 4;
       Place E 2 4 0;
       Place F 4 2 2;
       Place G 1 0 1;
       Place H 6 4 4]);
   ((12, 20),
     [Place A 4 0 2;
       Place B 1 4 1;
       Place C 2 3 0;
       Place D 1 1 4;
       Place E 0 3 3;
       Place F 1 2 0;
       Place G 3 4 4;
       Place H 0 0 0]);
   ((12, 21),
     [Place A 4 0 2;
       Place B 0 4 1;
       Place C 5 0 0;
       Place D 1 1 4;
       Place E 2 4 0;
       Place F 4 2 2;
       Place G 1 0 1;
       Place H 2 4 4]);
   ((12, 22),
     [Place A 7 5 0;
       Place B 1 1 3;
       Place C 0 0 2;
       Place D 0 0 0;
       Place E 2 1 0;
       Place F 0 4 0;
       Place G 3 4 4;
       Place H 3 2 5]);
   ((12, 23),
     [Place A 4 0 2;
       Place B 0 3 0;
       Place C 6 1 3;
       Place D 1 1 1;
       Place E 0 0 0;
       Place F 7 4 3;
       Place G 3 5 0;
       Place H 4 2 5]);
   ((12, 24),
     [Place A 7 0 2;
       Place B 1 2 5;
       Place C 0 0 0;
       Place D 3 3 1;
       Place E 2 4 0;
       Place F 4 4 3;
       Place G 0 2 2;
       Place H 7 1 0]);
   ((12, 25),
     [Place A 4 0 2;
       Place B 1 4 0;
       Place C 5 0 0;
       Place D 1 1 4;
       Place E 0 4 2;
       Place F 4 2 2;
       Place G 1 0 1;
       Place H 6 4 4]);
   ((12, 26),
     [Place A 4 0 2;
       Place B 0 4 1;
       Place C 5 0 0;
       Place D 1 1 4;
       Place E 2 4 0;
       Place F 4 2 2;
       Place G 1 0 1;
       Place H 1 4 4]);
   ((12, 27),
     [Place A 4 0 2;
       Place B 1 4 1;
       Place C 2 3 0;
       Place D 1 1 4;
       Place E 0 3 3;
       Place F 1 2 0;
       Place G 0 4 4;
       Place H 0 0 0]);
   ((12, 28),
     [Place A 4 0 2;
       Place B 0 4 1;
       Place C 5 0 0;
       Place D 1 1 4;
       Place E 2 4 0;
       Place F 4 2 2;
       Place G 1 0 1;
       Place H 0 4 4]);
   ((12, 29),
     [Place A 4 0 2;
       Place B 0 3 0;
       Place C 3 2 5;
       Place D 1 1 1;
       Place E 0 0 0;
       Place F 3 1 4;
       Place G 3 4 3;
       Place H 1 5 0]);
   ((12, 30),
     [Place A 4 0 2;
       Place B 0 3 0;
       Place C 6 1 3;
       Place D 1 1 1;
       Place E 0 0 0;
       Place F 7 4 3;
       Place G 0 5 0;
       Place H 4 2 5]);
   ((12, 31),
     [Place A 4 0 2;
       Place B 0 3 0;
       Place C 3 2 5;
       Place D 1 1 1;
       Place E 0 0 0;
       Place F 3 1 4;
       Place G 3 4 3;
       Place H 0 5 0])].

Example solution_table_has_366_entries :
  length solution_table = 366.
Proof. vm_compute. reflexivity. Qed.

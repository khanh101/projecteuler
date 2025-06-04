import ProjectEuler.Iterator

namespace P6

  open Iterator


  partial def run (_: Unit): String :=
    let l: List Nat := ((natural.drop_atmost 1).take_atmost 100).array.toList
    let sum_of_squares :=
      ((make_iterator_from_list l).map (λ (x: Nat) => x*x)).reduce (λ (x: Nat)(y: Nat) => some (x + y)) 0
    let sum :=
      (make_iterator_from_list l).reduce (λ (x: Nat)(y: Nat) => some (x + y)) 0
    let square_of_sum := sum * sum

    (Int.ofNat sum_of_squares - Int.ofNat square_of_sum).natAbs.repr
end P6

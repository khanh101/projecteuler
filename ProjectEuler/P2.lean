import ProjectEuler.Iterator

namespace P2
  open Iterator

  def fibonacci: Iterator (Nat × Nat) Nat :=
    let next (pair: Nat × Nat): Option ((Nat × Nat) × Nat) :=
      let (a, b) := pair
      some ((b, a + b), a)

    let state: Nat × Nat := (1, 2)
    make_iterator next state

  #eval (fibonacci.take_atmost 10).array -- should be 10 fibonacci numbers

  partial def run (_: Unit): String :=
    let s := fibonacci
    let s := s.filter ((λ (x: Nat) => x % 2 == 0)) -- keep even numbers only
    let rec loop {α} (s: Iterator α Nat) (sum: Nat): Nat :=
      match s.next with
        | some (s, x) =>
          if x ≤ 4000000
            then
              let sum := sum + x
              loop s sum
            else
              sum
        | none => 0 -- unreachable
    (loop s 0).repr

end P2

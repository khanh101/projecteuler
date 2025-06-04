import ProjectEuler.Iterator

namespace P2
  open Iterator


  def fibonacci: Iterator (Nat × Nat) Nat := {
    _next := ((λ (s: Nat × Nat) =>
      let (a, b) := s
      some ((b, a + b), a)
    ): Nat × Nat → Option ((Nat × Nat) × Nat))
    _state := (1, 2)
  }

  #eval let (_, x) := fibonacci.take 10; x -- should be 10 fibonacci numbers

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

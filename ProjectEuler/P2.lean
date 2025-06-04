import ProjectEuler.Iterator

namespace P2
  open Tools


  def fibonacci: Iterator (Nat × Nat) Nat :=
    Iterator.mk (1, 2) ((λ (s: Nat × Nat) =>
      let (a, b) := s
      some ((b, a + b), a)
    ): Nat × Nat → Option ((Nat × Nat) × Nat))

  #eval let (_, x) := fibonacci.take 10; x -- should be 10 fibonacci numbers



end P2

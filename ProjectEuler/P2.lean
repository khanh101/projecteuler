import ProjectEuler.Iterator

namespace P2
  open Tools


  def fibonacci: Iterator (Nat × Nat) Nat :=
    Iterator.mk (1, 2) ((λ (s: Nat × Nat) =>
      let (a, b) := s
      some ((b, a + b), a)
    ): Nat × Nat → Option ((Nat × Nat) × Nat))

  #eval fibonacci.take 2

end P2

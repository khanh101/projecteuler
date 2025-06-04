import ProjectEuler.Iter

namespace P2
  structure Iterator (α: Type) (β: Type) where
    state: α
    next: α → Option (α × β)

  def Iterator.map (i: Iterator α β) (f: β → γ): Iterator α γ :=
    let next (s: α): Option (α × γ) :=
      match i.next s with
        | some (s1, b) => (s1, f b)
        | none => none
    Iterator.mk i.state next

  -- TODO cannot prove termination for now
  def Iterator.filter (i: Iterator α β) (f: β → Bool): Iterator α β :=
    let rec loop (s: α): Option (α × β) :=
      match i.next s with
        | some (s1, b) =>
          if (f b)
            then (s1, b)
            else loop s1
        | none => none
    decreasing_by all_goals sorry
    Iterator.mk i.state loop

  -- TODO implement reduce, flatMap


  def fibonacci: Iterator (Nat × Nat) Nat :=
    Iterator.mk (1, 2) ((λ (s: Nat × Nat) =>
      let (a, b) := s
      some ((b, a + b), a)
    ): Nat × Nat → Option ((Nat × Nat) × Nat))

  def run (_: Unit): String :=

end P2

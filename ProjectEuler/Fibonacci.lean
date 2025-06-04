import ProjectEuler.Iterator

namespace Fibonacci
  open Iterator

  def fibonacci: Iterator (Nat × Nat) Nat :=
    let next (pair: Nat × Nat): Option ((Nat × Nat) × Nat) :=
      let (a, b) := pair
      some ((b, a + b), a)

    let state: Nat × Nat := (1, 2)
    make_iterator next state

  #eval (fibonacci.take_atmost 10).array -- should be 10 fibonacci numbers

end Fibonacci

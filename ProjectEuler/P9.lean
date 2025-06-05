import ProjectEuler.Output

namespace P9

  -- m > n > 0
  def pythagorean (m: Nat) (n: Nat): Nat × Nat × Nat :=
    let a := m^2 - n^2
    let b := 2 * m * n 
    let c := m^2 + n^2
    (a, b, c)
  


  partial def run (_: Unit): Output :=
    sorry


end P9
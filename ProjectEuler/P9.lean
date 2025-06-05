import ProjectEuler.Output



namespace P9

  -- m > n > 0
  def get_pythagorean (m: Nat) (n: Nat): Nat × Nat × Nat :=
    let a := m^2 - n^2
    let b := 2 * m * n 
    let c := m^2 + n^2
    (a, b, c)

  partial def run (_: Unit): Output :=
    let rec loop (m: Nat): Nat × Nat :=
      if 2*m*m > 1000 then (0, 0) else -- stop condition
      let n: Nat := 1000 / (2 * m) - m -- a + b + c = 2m(m+n)
      let cond := m > n && n > 0 && 2 * m * (m + n) == 1000
      if cond then (m, n)
      else loop (m+1)
    
    let (m, n) := loop 1
    let (a, b, c) := get_pythagorean m n

    Output.Nat (a*b*c)

end P9
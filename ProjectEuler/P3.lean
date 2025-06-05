import ProjectEuler.Output

import ProjectEuler.Iterator
import ProjectEuler.Prime

namespace P3

  open Iterator
  open Prime


  partial def run (_: Unit): Output :=
    let ps := prime
    let n := 600851475143

    let reduce (largest_factor: Nat) (p: Nat): Option Nat :=
      if p * p > n then none -- stop iteration here
        else
        if n % p == 0 then some p -- previous largest factor
        else some largest_factor

    Output.Nat (ps.reduce reduce 0)
end P3

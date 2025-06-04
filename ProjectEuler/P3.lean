import ProjectEuler.Iterator

namespace P3

  open Iterator

  -- prime sieve
  partial def prime: Iterator Nat Nat :=
    let state := 0

  partial def run (_: Unit): String :=
    let ps := prime
    let n := 600851475143

    let reduce (largest_factor: Nat) (p: Nat): Option Nat :=
      if p * p > n then none -- stop iteration here
        else
        if (n % p) == 0 then some largest_factor -- previous largest factor
        else some p

    (ps.reduce reduce 0).repr

end P3

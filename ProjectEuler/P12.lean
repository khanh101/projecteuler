import ProjectEuler.Output
import ProjectEuler.Prime
import ProjectEuler.Iterator



namespace P12
  open Iterator
  open Prime


  partial def triangle_number (n: Nat): Nat :=
    let rec loop (n: Nat) (a: Nat) : Nat :=
      if n == 0 then a else
        loop (n-1) (a + n)

    loop n 0

  -- n ≥ 2
  partial def get_prime_factor (n: Nat) : Nat :=
    let rec loop {α} (n: Nat) (ps: Iterator α Nat) : Nat :=
      match ps.next with
        | some (ps, p) =>
          if n % p == 0 then p else loop n ps
        | none => 0 -- unreachable
    loop n prime


  partial def factorize (n: Nat): Array Nat :=
    if n ≤ 1 then #[] else

    let rec loop (n: Nat) (ps: Array Nat): Array Nat :=
      if n ≤ 1 then ps else
        let p := get_prime_factor n
        loop (n/p) (ps.push p)

    loop n #[]


  -- number of factors is precisely the number of subsets of the set of factors
  def num_factors (ps: Array Nat): Nat :=
    let m := ps.size
    let twos := (Iterator.replicate 2).take_atmost m -- list of 2s of length m
    twos.reduce ((λ x y => some (x * y)): Nat → Nat → Option Nat) 1

  #eval factorize 28
  #eval num_factors (factorize 28)


  partial def run (_: Unit): Output :=
    let p := prime
    -- TODO
    Output.Nat 0

end P12

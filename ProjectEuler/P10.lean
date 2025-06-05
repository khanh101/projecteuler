import ProjectEuler.Output
import ProjectEuler.Prime


namespace P10
  open Prime
  
  partial def run (_: Unit): Output :=
    let s := prime.reduce ((λ (s: Nat) (p: Nat) =>
      if p ≥ 2000000 then none else
      some (s + p)
    ): Nat → Nat → Option Nat) 0

    Output.Nat s

end P10
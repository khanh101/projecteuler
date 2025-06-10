import ProjectEuler.Output
import ProjectEuler.Iterator


namespace P14
  open Iterator


  def collatz (n: Nat): Iterator Nat Nat :=
    let next (n: Nat): Option (Nat × Nat) :=
      if n == 1 then none else
      if (n % 2) == 0 then
        let m := n / 2
        some (m, m)
      else
        let m := 3 * n + 1
        some (m, m)
    make_iterator next n

  -- TODO - cache this function
  -- def get_collatz_chain_length (n: Nat): Nat :=
  --  1 + (collatz n).size

  partial def get_collatz_chain_length (size: Nat): Array Nat :=
    let rec write (a: Array Nat) (n: Nat): (Array Nat) × Nat :=
      if n < a.size && a[n]! > 0 then (a, a[n]!) else
        if n ≤ 1 then (a.set! n 1, 1) else
        let m := if (n % 2) == 0 then n / 2 else 3 * n + 1
        let (a, l) := write a m
        (a.set! n (l+1), l+1)
        
    let rec loop (a: Array Nat) (n: Nat): Array Nat :=
      if n ≥ a.size then a else
        let (a, _) := write a n
        loop a (n+1)
    
    loop (Array.replicate size 0) 0

  #eval (get_collatz_chain_length 14).enum

  partial def run (_: Unit): Output :=
    let a: Array Nat := get_collatz_chain_length 1000000
    
    Output.Nat 0

end P14

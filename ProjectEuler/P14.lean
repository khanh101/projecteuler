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
  def get_collatz_chain_length (n: Nat): Nat :=
    1 + (collatz n).size

  #eval get_collatz_chain_length 1

  partial def run (_: Unit): Output :=
    let natural_under_1000000 := natural.take_atmost 1000000

    let max_type := Nat × Nat -- index × value
    let m := natural_under_1000000.reduce ((λ m j =>
      let (_, v) := m
      let l := get_collatz_chain_length j
      if l > v then
        some (j, l)
      else
        some m
    ): max_type → Nat → Option max_type) (0, 0)

    let (i, _) := m
    Output.Nat i

end P14

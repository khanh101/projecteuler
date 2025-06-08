import ProjectEuler.Output

namespace P12

  partial def triangle_number (n: Nat): Nat :=
    let rec loop (n: Nat) (a: Nat) : Nat :=
      if n == 0 then a else
        loop (n-1) (a + n)

    loop n 0

  partial def run (_: Unit): Output :=
    -- TODO
    Output.Nat 0

end P12

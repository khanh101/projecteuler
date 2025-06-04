import ProjectEuler.Iterator
import ProjectEuler.Prime

namespace P7

  open Iterator
  open Prime

  partial def run (_: Unit): String :=
    let ps := prime
    let a: Array Nat := ((ps.drop_atmost 10000).take_atmost 1).array
    match a[0]? with
      | some p => p.repr
      | none => "error"

end P7

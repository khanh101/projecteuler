import ProjectEuler.Output

import ProjectEuler.Iterator
import ProjectEuler.Prime

namespace P7

  open Iterator
  open Prime

  partial def run (_: Unit): Output :=
    let ps := prime
    let a: Array Nat := ((ps.drop_atmost 10000).take_atmost 1).array
    match a[0]? with
      | some p => Output.Nat p
      | none => Output.Str "error"

end P7

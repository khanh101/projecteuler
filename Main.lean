import ProjectEuler.P1

def run (n: Nat): String :=
  match n with
    | 1 => P1.run ()
    | _ => s!"no solution for problem {n}."


def main (args : List String) : IO UInt32 := do
  match args with
  | x :: _ =>
    match String.toNat? x with
    | some n => IO.println (run n)
    | none   => IO.println s!"could not parse '{x}' as a natural number."
  | [] => IO.println "please provide a number as the first argument."
  return 0

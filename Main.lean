import ProjectEuler.Output
import ProjectEuler.P1
import ProjectEuler.P2
import ProjectEuler.P3
import ProjectEuler.P6
import ProjectEuler.P7
import ProjectEuler.P9
import ProjectEuler.P10
import ProjectEuler.P14


def run (n: Nat): Output :=
  match n with
    | 1 => P1.run ()
    | 2 => P2.run ()
    | 3 => P3.run ()
    | 6 => P6.run ()
    | 7 => P7.run ()
    | 9 => P9.run ()
    | 10 => P10.run ()
    | 14 => P14.run ()
    | _ => Output.Str s!"no solution for problem {n}."



def main (args : List String) : IO UInt32 := do
  match args with
  | x :: _ =>
    match String.toNat? x with
    | some n => IO.println (run n).repr
    | none   => IO.println s!"could not parse '{x}' as a natural number."
  | [] => IO.println "please provide a number as the first argument."
  return 0

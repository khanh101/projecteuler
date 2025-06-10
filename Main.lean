import ProjectEuler.Output
import ProjectEuler.P1
import ProjectEuler.P2
import ProjectEuler.P3
import ProjectEuler.P6
import ProjectEuler.P7
import ProjectEuler.P9
import ProjectEuler.P10
import ProjectEuler.P14
import Std


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


namespace DP 
  -- cache_output returns a function that saves the outputs of function with a filter
  def cache_output [BEq α] [Hashable α] (function: α → β) (filter: α → Bool) : (Std.HashMap α β) → α → (Std.HashMap α β) × β :=
    let rec function_with_cache (h: Std.HashMap α β) (a: α) : (Std.HashMap α β) × β :=
      if !(filter a) then 
        let b := function a
        (h, b)
      else
        match h[a]? with
          | some b => (h, b)
          | none =>
            let b := function a
            (h.insert a b, b)

    function_with_cache

  partial def fibonacci_nocache (n: Nat): Nat :=
    if n ≤ 1 then
      n
    else
      (fibonacci_nocache (n-1)) + (fibonacci_nocache (n-2))
  

end DP


def main (args : List String) : IO UInt32 := do
  IO.println (DP.fibonacci_nocache 5).repr
  match args with
  | x :: _ =>
    match String.toNat? x with
    | some n => IO.println (run n).repr
    | none   => IO.println s!"could not parse '{x}' as a natural number."
  | [] => IO.println "please provide a number as the first argument."
  return 0

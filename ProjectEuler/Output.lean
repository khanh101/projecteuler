inductive Output where
  | Nat: Nat → Output
  | Int: Int → Output
  | Str: String → Output

def Output.repr (o: Output): String :=
  match o with
    | Output.Nat o => o.repr
    | Output.Int o => o.repr
    | Output.Str o => o

inductive Output where
  | isNat: Nat → Output
  | isInt: Int → Output
  | isStr: String → Output

def Output.repr (o: Output): String :=
  match o with
    | Output.isNat o => o.repr
    | Output.isInt o => o.repr
    | Output.isStr o => o

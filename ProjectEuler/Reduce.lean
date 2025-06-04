def reduce (actions: List β) (update: α → β → α) (state: α): α :=
  match actions with
    | [] => state
    | action :: actions => reduce actions update (update state action)

def printListInt (l: List Int): String :=
  let s := reduce l (λ (acc: String)(item: Int) => acc ++ item.repr ++ ", ") ""
  "[" ++ s ++ "]"

#eval printListInt [1, 2, 3]

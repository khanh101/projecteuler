namespace Tools
structure Iterator (α: Type) (β: Type) where
  _state: α
  _next: α → Option (α × β)

def Iterator.next (i: Iterator α β): Option ((Iterator α β) × β) :=
  match i._next i._state with
    | some (s1, b) => some ((Iterator.mk s1 i._next), b)
    | none => none

partial def Iterator.take (i: Iterator α β) (n: Nat): (Option (Iterator α β)) × Array β :=
  let rec loop (i: Iterator α β) (n: Nat) (a: Array β): (Option (Iterator α β)) × Array β :=
    match n with
      | 0 => (i, a)
      | _ =>
        match i._next i._state with
          | some (s1, b) =>
            let a := a.push b
            loop (Iterator.mk s1 i._next) (n-1) a
          | none =>
            (none, a)
  loop i n #[]

def Iterator.map (i: Iterator α β) (f: β → γ): Iterator α γ :=
  let next (s: α): Option (α × γ) :=
    match i._next s with
      | some (s1, b) => some (s1, f b)
      | none => none
  Iterator.mk i._state next

partial def Iterator.filter (i: Iterator α β) (f: β → Bool): Iterator α β :=
  let rec loop (s: α): Option (α × β) :=
    match i._next s with
      | some (s1, b) =>
        if (f b)
          then some (s1, b)
          else loop s1
      | none => none
  Iterator.mk i._state loop


partial def Iterator.reduce (i: Iterator α β) (f: γ → β → γ) (a: γ): γ :=
  let rec loop (i: Iterator α β) (f: γ → β → γ) (a: γ): γ :=
    match i._next i._state with
      | some (s1, b) =>
        let a := f a b
        loop (Iterator.mk s1 i._next) f a
      | none => a
  loop i f a

partial def Iterator.flat_map (i: Iterator α β) (f: β → Iterator γ δ): Iterator ((Iterator α β) × Option (Iterator γ δ)) δ :=
  let state_type := (Iterator α β) × Option (Iterator γ δ)
  let rec loop (s: state_type): Option (state_type × δ) :=
    let (i1, i2) := s
    match i2 with
      | some i2 =>
        match i2._next i2._state with
          | some (s2, d) =>
            some ((i1, some (Iterator.mk s2 i2._next)), d)
          | _ =>
            loop (i1, none)
      | none =>
        match i1._next i1._state with
          | some (s1, b) =>
            let i2 := f b
            loop (Iterator.mk s1 i1._next, i2)
          | none => none
  Iterator.mk (i, none) loop

def make_iterator_from_list (l: List α): Iterator (List α) α :=
  let state_type := List α
  let rec loop (s: state_type): Option (state_type × α) :=
    match s with
      | [] => none
      | head :: tail => some (tail, head)

  Iterator.mk l loop

partial def Iterator.toArray (i: Iterator α β): Array β :=
  let rec loop (i: Iterator α β) (bs: Array β): Array β :=
    match i._next i._state with
      | some (s1, b) =>
        let bs := bs.push b
        loop (Iterator.mk s1 i._next) bs
      | none => bs

  loop i #[]

  namespace test
    def a := make_iterator_from_list [1, 2, 3, 4]

    #check a
    #eval a.toArray

    #eval let (_, x) := a.take 0; x
    #eval let (_, x) := a.take 2; x
    #eval let (_, x) := a.take 5; x

    #check a.map ((λ x => x+1): Nat → Nat)
    #eval (a.map ((λ x => x+1): Nat → Nat)).toArray

    #check a.filter ((λ x => x % 2 == 0))
    #eval (a.filter ((λ x => x % 2 == 0))).toArray

    #check a.reduce ((λ (s: String)(x: Nat) => s ++ x.repr ++ ",")) ""
    #eval a.reduce ((λ (s: String)(x: Nat) => s ++ x.repr ++ ",")) ""

    #check a.flat_map ((λ (x: Nat) => make_iterator_from_list (List.replicate x x)))
    #eval (a.flat_map ((λ (x: Nat) => make_iterator_from_list (List.replicate x x)))).toArray

  end test

end Tools

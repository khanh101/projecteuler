namespace Tools
structure Iterator (α: Type) (β: Type) where
  _next: α → Option (α × β)
  _state: α

def Iterator.next (i: Iterator α β): Option ((Iterator α β) × β) :=
  match i._next i._state with
    | some (s1, b) => some ({_next := i._next, _state := s1}, b)
    | none => none

partial def Iterator.take (i: Iterator α β) (n: Nat): (Option (Iterator α β)) × Array β :=
  let rec loop (i: Iterator α β) (n: Nat) (a: Array β): (Option (Iterator α β)) × Array β :=
    match n with
      | 0 => (i, a)
      | _ =>
        match i._next i._state with
          | some (s1, b) =>
            let a := a.push b
            loop {_next := i._next, _state := s1} (n-1) a
          | none =>
            (none, a)
  loop i n #[]

def Iterator.map (i: Iterator α β) (f: β → γ): Iterator α γ :=
  let next (s: α): Option (α × γ) :=
    match i._next s with
      | some (s1, b) => some (s1, f b)
      | none => none
  {_next := next, _state := i._state}

partial def Iterator.filter (i: Iterator α β) (f: β → Bool): Iterator α β :=
  let rec loop (s: α): Option (α × β) :=
    match i._next s with
      | some (s1, b) =>
        if (f b)
          then some (s1, b)
          else loop s1
      | none => none
  {_next := loop, _state := i._state}

partial def Iterator.fold (i: Iterator α β) (f: γ → β → γ) (a: γ): Iterator (γ × Iterator α β) γ :=
  let state := γ × Iterator α β
  let rec loop (s: state): Option (state × γ) :=
    let (a, i) := s
    match i._next i._state with
      | some (s1, b) =>
        let a := f a b
        some ((a, {_next := i._next, _state := s1}), a)
      | none => none

  {_next := loop, _state := (a, i)}

partial def Iterator.last (i: Iterator α β): Option β :=
  match i._next i._state with
    | some (s1, b) =>
        let rec loop (i: Iterator α β) (b: β): β :=
          match i._next i._state with
            | some (s1, b) => loop {_next := i._next, _state := s1} b
            | none => b
        loop {_next := i._next, _state := s1} b
    | none => none

partial def Iterator.reduce (i: Iterator α β) (f: γ → β → γ) (a: γ): γ :=
  match (i.fold f a).last with
    | some a => a
    | none => a

partial def Iterator.flat_map (i: Iterator α β) (f: β → Iterator γ δ): Iterator ((Iterator α β) × Option (Iterator γ δ)) δ :=
  let state_type := (Iterator α β) × Option (Iterator γ δ)
  let rec loop (s: state_type): Option (state_type × δ) :=
    let (i1, i2) := s
    match i2 with
      | some i2 =>
        match i2._next i2._state with
          | some (s2, d) =>
            some ((i1, some {_next := i2._next, _state := s2}), d)
          | _ =>
            loop (i1, none)
      | none =>
        match i1._next i1._state with
          | some (s1, b) =>
            let i2 := f b
            loop ({_next := i._next, _state := s1}, i2)
          | none => none
  {_next := loop, _state := (i, none)}

def make_iterator_from_list (l: List α): Iterator (List α) α :=
  let state_type := List α
  let rec loop (s: state_type): Option (state_type × α) :=
    match s with
      | [] => none
      | head :: tail => some (tail, head)

  {_next := loop, _state := l}

partial def Iterator.toArray (i: Iterator α β): Array β :=
  let rec loop (i: Iterator α β) (bs: Array β): Array β :=
    match i._next i._state with
      | some (s1, b) =>
        let bs := bs.push b
        loop {_next := i._next, _state := s1} bs
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

    #check a.fold ((λ (s: String)(x: Nat) => s ++ x.repr ++ ",")) ""
    #eval (a.fold ((λ (s: String)(x: Nat) => s ++ x.repr ++ ",")) "").toArray

    #check a.flat_map ((λ (x: Nat) => make_iterator_from_list (List.replicate x x)))
    #eval (a.flat_map ((λ (x: Nat) => make_iterator_from_list (List.replicate x x)))).toArray

    #check a.last
    #eval a.last

  end test

end Tools

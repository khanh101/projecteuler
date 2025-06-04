namespace Iterator
structure Iterator (α: Type)(β: Type) where
  _next: α → Option (α × β)
  _state: α

def natural: Iterator Nat Nat :=
  {_next := (λ (s: Nat) => some (s+1, s)), _state := 0}

def make_iterator (next: α → Option (α × β)) (state: α): Iterator α β :=
  {_next := next, _state := state}

def make_iterator_from_list (l: List α): Iterator (List α) α :=
  let state_type := List α
  let rec loop (s: state_type): Option (state_type × α) :=
    match s with
      | [] => none
      | head :: tail => some (tail, head)

  {_next := loop, _state := l}

partial def Iterator.take_all (i: Iterator α β): Array β :=
  let rec loop (i: Iterator α β) (bs: Array β): Array β :=
    match i._next i._state with
      | some (s1, b) =>
        let bs := bs.push b
        loop {_next := i._next, _state := s1} bs
      | none => bs

  loop i #[]

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

partial def Iterator.last (i: Iterator α β): Option β :=
  match i._next i._state with
    | some (s1, b) =>
        let rec loop (i: Iterator α β) (b: β): β :=
          match i._next i._state with
            | some (s1, b) => loop {_next := i._next, _state := s1} b
            | none => b
        loop {_next := i._next, _state := s1} b
    | none => none

partial def Iterator.drop (i: Iterator α β) (n: Nat): Option (Iterator α β) :=
  if n == 0 then i else
    match i.next with
      | some (i , _) => i.drop (n-1)
      | none => none

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

partial def Iterator.fold (i: Iterator α β) (f: γ → β → Option γ) (a: γ): Iterator (γ × Iterator α β) γ :=
  let state := γ × Iterator α β
  let rec loop (s: state): Option (state × γ) :=
    let (a, i) := s
    match i._next i._state with
      | some (s1, b) =>
        let a := f a b
        match a with
          | some a => some ((a, {_next := i._next, _state := s1}), a)
          | none => none
      | none => none

  {_next := loop, _state := (a, i)}

partial def Iterator.reduce (i: Iterator α β) (f: γ → β → Option γ) (a: γ): γ :=
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


  namespace test
    def a := make_iterator_from_list [1, 2, 3, 4]

    #check a
    #eval a.take_all

    #eval let (_, x) := a.take 0; x
    #eval let (_, x) := a.take 2; x
    #eval let (_, x) := a.take 5; x

    #check a.map ((λ x => x+1): Nat → Nat)
    #eval (a.map ((λ x => x+1): Nat → Nat)).take_all

    #check a.filter ((λ x => x % 2 == 0))
    #eval (a.filter ((λ x => x % 2 == 0))).take_all

    -- join but stop if ≥ 4
    def join (s: String) (x: Nat): Option String :=
      if x ≥ 4 then none else
        s ++ x.repr ++ ","

    #check a.reduce join ""
    #eval a.reduce join ""

    #check a.fold join ""
    #eval (a.fold join "").take_all

    #check a.flat_map ((λ (x: Nat) => make_iterator_from_list (List.replicate x x)))
    #eval (a.flat_map ((λ (x: Nat) => make_iterator_from_list (List.replicate x x)))).take_all

    #check a.last
    #eval a.last

  end test

end Iterator

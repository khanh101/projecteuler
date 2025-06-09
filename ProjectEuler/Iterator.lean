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

def replicate (b: β): Iterator β β :=
  let next (b: β): Option (β × β) := (b, b)
  {_next := next, _state := b}

def Iterator.next (i: Iterator α β): Option ((Iterator α β) × β) :=
  match i._next i._state with
    | some (s1, b) => some ({_next := i._next, _state := s1}, b)
    | none => none

def Iterator.prepend (i: Iterator α β) (l: List β): Iterator ((Iterator α β) × (List β)) β :=
  let state_type := (Iterator α β) × (List β)
  let state: state_type := (i, l)
  let next (s: state_type): Option (state_type × β) :=
    let (i, l) := s
    match l with
      | [] =>
        match i.next with
          | some (i1, b) => some ((i1, l), b)
          | none => none
      | head :: l => some ((i, l), head)

  {_next := next, _state := state}

partial def Iterator.array (i: Iterator α β): Array β :=
  let rec loop (i: Iterator α β) (bs: Array β): Array β :=
    match i.next with
      | some (i1, b) => loop i1 (bs.push b)
      | none => bs

  loop i #[]

partial def Iterator.size (i: Iterator α β): Nat :=
  let rec loop (i: Iterator α β) (n: Nat): Nat :=
    match i.next with
      | some (i1, _) => loop i1 (n+1)
      | none => n
  loop i 0

partial def Iterator.last (i: Iterator α β): Option β :=
  match i.next with
    | some (i1, b) =>
      let rec loop (i1: Iterator α β) (b: β): β :=
        match i1.next with
          | some (i2, b2) => loop i2 b2
          | none => b
      loop i1 b
    | none => none

partial def Iterator.drop (i: Iterator α β) (n: Nat): Option (Iterator α β) :=
  if n == 0 then i else
    match i.next with
      | some (i , _) => i.drop (n-1)
      | none => none

partial def Iterator.drop_atmost (i: Iterator α β) (n: Nat): Iterator α β :=
  if n == 0 then i else
    match i.next with
      | some (i , _) => i.drop_atmost (n-1)
      | none => i

partial def Iterator.take_atmost (i: Iterator α β) (n: Nat): Iterator ((Iterator α β) × Nat) β :=
  let state_type := (Iterator α β) × Nat
  let state := (i, n)
  let next (s: state_type): Option (state_type × β) :=
    let (i, n) := s
    if n == 0 then none else
      match i.next with
        | some (i1, b) => some ((i1, n-1), b)
        | none => none
  {_next := next, _state := state}



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
  let state_type := γ × Iterator α β
  let rec loop (s: state_type): Option (state_type × γ) :=
    let (a, i) := s
    match i.next with
      | some (i1, b) =>
        match f a b with
          | some a => some ((a, i1), a)
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

    #eval (natural.take_atmost 10).array
    #eval ((natural.filter (λ (x: Nat) => x % 2 == 0)).take_atmost 10).array

    #eval ((replicate 5).take_atmost 10).array


    def a := make_iterator_from_list [1, 2, 3, 4]

    #check a
    #eval a.array

    #eval (a.take_atmost 0).array
    #eval (a.take_atmost 2).array
    #eval (a.take_atmost 5).array


    #check a.last
    #eval a.last

    #check a.prepend [9, 8, 7]
    #eval (a.prepend [9, 8, 7]).array

    #eval (a.drop_atmost 0).array
    #eval (a.drop_atmost 2).array
    #eval (a.drop_atmost 5).array



    #check a.map ((λ x => x*2): Nat → Nat)
    #eval (a.map ((λ x => x*2): Nat → Nat)).array

    #check a.filter ((λ x => x % 2 == 0))
    #eval (a.filter ((λ x => x % 2 == 0))).array

    -- join but stop if ≥ 4
    def join (s: String) (x: Nat): Option String :=
      if x ≥ 4 then none else
        s ++ x.repr ++ ","

    #check a.reduce join ""
    #eval a.reduce join ""

    #check a.fold join ""
    #eval (a.fold join "").array

    #check a.flat_map (λ (x: Nat) => (replicate x).take_atmost x)
    #eval (a.flat_map (λ (x: Nat) => (replicate x).take_atmost x)).array



  end test

end Iterator

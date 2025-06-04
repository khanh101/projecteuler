structure Iterator (α: Type) (β: Type) where
  state: α
  next: α → Option (α × β)

def Iterator.get_next(i: Iterator α β): Option (α × β) := i.next i.state

def Iterator.map (i: Iterator α β) (f: β → γ): Iterator α γ :=
  let next (s: α): Option (α × γ) :=
    match i.next s with
      | some (s1, b) => (s1, f b)
      | none => none
  Iterator.mk i.state next

partial def Iterator.filter (i: Iterator α β) (f: β → Bool): Iterator α β :=
  let rec loop (s: α): Option (α × β) :=
    match i.next s with
      | some (s1, b) =>
        if (f b)
          then (s1, b)
          else loop s1
      | none => none
  Iterator.mk i.state loop


partial def Iterator.reduce (i: Iterator α β) (f: γ → β → γ) (a: γ): γ :=
  let rec loop (i: Iterator α β) (f: γ → β → γ) (a: γ): γ :=
    match i.next i.state with
      | some (s1, b) =>
        let a := f a b
        loop (Iterator.mk s1 i.next) f a
      | none => a
  loop i f a

partial def Iterator.flat_map (i: Iterator α β) (f: β → Iterator γ δ): Iterator ((Iterator α β) × Option (Iterator γ δ)) δ :=
  let state := (Iterator α β) × Option (Iterator γ δ)
  let rec loop (s: state): Option (state × δ) :=
    let (i1, i2) := s
    match i2 with
      | some i2 =>
        match i2.next i2.state with
          | some (s2, d) =>
            some ((i1, some (Iterator.mk s2 i2.next)), d)
          | _ =>
            loop (i1, none)
      | none =>
        match i1.next i1.state with
          | some (s1, b) =>
            let i2 := f b
            loop (Iterator.mk s1 i1.next, i2)
          | none => none
  Iterator.mk (i, none) loop

def make_iterator_from_list (l: List α): Iterator (List α) α :=
  let state := List α
  let rec loop (s: state): Option (state × α) :=
    match s with
      | [] => none
      | head :: tail => some (tail, head)

  Iterator.mk l loop

partial def Iterator.toArray (i: Iterator α β): Array β :=
  let rec loop (i: Iterator α β) (bs: Array β): Array β :=
    match i.next i.state with
      | some (s1, b) =>
        let bs := bs.push b
        let i := Iterator.mk s1 i.next
        loop i bs
      | none => bs

  loop i #[]



#eval (make_iterator_from_list [1, 2, 3, 4]).toArray

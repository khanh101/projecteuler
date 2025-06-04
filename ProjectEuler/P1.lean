namespace P1
  def run (_: Unit): String :=
    let l := List.range 1000 -- List of numbers from 0 to 999
    let l := l.filter (λ (n: Nat) => (n % 3 == 0) || (n % 5 == 0))
    let s := l.foldl (λ (x: Nat)(y: Nat) => x+y) 0
    s.repr
end P1

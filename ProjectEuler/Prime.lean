import ProjectEuler.Iterator

namespace Prime

  open Iterator

  -- prime test for c >= 3
  private partial def odd_prime_test (c: Nat): Bool :=
    let rec loop (m: Nat): Bool :=
      if m * m > c then true else
      if c % m == 0 then false else
        loop (m+2)

    loop 3

  private partial def next_odd_prime (c: Nat): Nat :=
    if odd_prime_test c then c else
      next_odd_prime (c+2)


  partial def prime :=
    let prime_ge3: Iterator Nat Nat :=
      let last_prime: Nat := 3
      let next (last_prime: Nat): Option (Nat × Nat) :=
        let next_prime := next_odd_prime (last_prime + 2)
        some (next_prime, next_prime)

      make_iterator next last_prime

    prime_ge3.insert [2, 3]

  #eval (prime.take_atmost 10).array -- should be 10 fibonacci numbers

end Prime

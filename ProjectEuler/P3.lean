import ProjectEuler.Iterator

namespace P3

  open Iterator


  -- prime test for c >= 3
  partial def odd_prime_test (c: Nat): Bool :=
    let rec loop (m: Nat): Bool :=
      if m * m > c then true else
      if c % m == 0 then false else
        loop (m+2)

    loop 3

  partial def next_odd_prime (c: Nat): Nat :=
    if odd_prime_test c then c else
      next_odd_prime (c+2)


  partial def prime :=
    let prime_ge3: Iterator Nat Nat :=
      let last_prime: Nat := 3
      let next (last_prime: Nat): Option (Nat × Nat) :=
        let next_prime := next_odd_prime (last_prime + 2)
        some (next_prime, next_prime)

      make_iterator next last_prime

    prime_ge3.prepend [2, 3]

  #eval (prime.take_atmost 10).array -- should be 10 fibonacci numbers

  partial def run (_: Unit): String :=
    let ps := prime
    let n := 600851475143

    let reduce (largest_factor: Nat) (p: Nat): Option Nat :=
      if p * p > n then none -- stop iteration here
        else
        if n % p == 0 then some p -- previous largest factor
        else some largest_factor

    (ps.reduce reduce 0).repr

end P3

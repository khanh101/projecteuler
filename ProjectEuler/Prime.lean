import ProjectEuler.Iterator

namespace Prime

  open Iterator

  -- odd_non_unit : odd numbers but not a unit 3, 5, 7, 9, ...
  private def odd_non_unit := natural.map ((λ n => 2*n + 3): Nat → Nat)

  -- odd_prime_test : give n odd and n > 1, prime test
  private def odd_prime_test (n: Nat): Bool :=
    0 == odd_non_unit.reduce ((λ (num_factors: Nat) (m: Nat) =>
      if num_factors > 0 then none else
      if m * m > n then none else
      if n % m == 0 then
        some (num_factors + 1)
      else
        some num_factors
    ): Nat → Nat → Option Nat) 0

  def prime := (odd_non_unit.filter odd_prime_test).insert [2]

  #eval (prime.take_atmost 10).array -- should be 10 prime numbers

end Prime

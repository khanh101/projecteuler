import Std

namespace DP 
  -- cache_output returns a function that saves the outputs of function with a filter
  def cache_output [BEq α] [Hashable α] (function: α → β) (filter: α → Bool) : (Std.HashMap α β) → α → (Std.HashMap α β) × β :=
    let rec function_with_cache (h: Std.HashMap α β) (a: α) : (Std.HashMap α β) × β :=
      if !(filter a) then 
        let b := function a
        (h, b)
      else
        match h[a]? with
          | some b => (h, b)
          | none =>
            let b := function a
            (h.insert a b, b)

    function_with_cache

end DP
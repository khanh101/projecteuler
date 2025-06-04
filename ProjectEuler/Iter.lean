structure Iter (α: Type) where
  next: Unit → (Option α × Iter α)

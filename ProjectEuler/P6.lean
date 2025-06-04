import ProjectEuler.Iterator

namespace P6

  open Iterator

  def x := let (_, x) := natural.take 101; x[1:].array
  #eval x


  partial def run (_: Unit): String :=



    ""
end P6

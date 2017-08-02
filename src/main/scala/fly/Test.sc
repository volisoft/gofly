def subsets(set: List[String]) = {
  val subsets = List[String]()
  for {
    subset <- 0 until  Math.pow(2, set.size).toInt
    newSubset = List[String]()
    bit <- 0 to set.size if isBitFlipped(subset, bit)
  } yield set(bit)
}

def isBitFlipped(num: Int, bit: Int) = ((num >> bit) & 1) == 1

sub(List("a", "b", "c"))

def sub(set: List[String]): List[List[String]] = set match {
  case Nil => List(List())
  case x :: xs =>
    val subsets = sub(xs)
    val ns = for { s <- subsets } yield x :: s
    ns ++ subsets
}
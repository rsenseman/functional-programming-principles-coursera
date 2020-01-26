package funsets

object Main extends App {
  import FunSets._
  val newSet: FunSet = singletonSet(5)

  val set_A = (x:Int) => (x < 5) && (x > -5)
  val set_B = (x:Int) => (x < 10) && (x > 0)

  println(forall(set_A, (x:Int) => x > 0))
  println(forall(set_B, (x:Int) => x > 0))
  println()

  println(exists(set_A, (x) => x==5))
  println(exists(set_B, (x) => x==5))
  println()

  printSet(map(set_A, x => x*2))
  printSet(map(set_B, x => x*2))
}

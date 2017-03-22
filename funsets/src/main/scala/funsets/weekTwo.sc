import funsets.Main
type Set = Int => Boolean

val bound = 1000
def forall(s: Set, p: Int => Boolean): Boolean = {
  def iter(a: Int): Boolean = {
    if (s(a) && !p(a)) false
    else if (a > bound) true
    else iter(a+1)
  }
  iter(-bound)
}

/**
  * Returns whether there exists a bounded integer within `s`
  * that satisfies `p`.
  */
def exists(s: Set, p: Int => Boolean): Boolean = !forall(s, x => !p(x))
def singletonSet(elem: Int): Set = (x: Int) => elem == x
def map(s: Set, f: Int => Int): Set = (x: Int) => exists(s, (y : Int) => f(y) == x)

val s1:Set = (x:Int) => x>=0 && x < 7

val s2 = map(s1, x => 2*x)

s2(12)



abstract class Rexp
case object ZERO extends Rexp
case object ONE extends Rexp
case class CHAR(c: Char) extends Rexp
case class ALT(r1: Rexp, r2: Rexp) extends Rexp
case class SEQ(r1: Rexp, r2: Rexp) extends Rexp
case class STAR(r: Rexp) extends Rexp
case class NTIMES(r: Rexp, n: Int) extends Rexp

// implicit type conversion stuff
import scala.language.implicitConversions
import scala.language.reflectiveCalls

def charlist2rexp(s: List[Char]): Rexp = s match {
  case Nil => ONE
  case c::Nil => CHAR(c)
  case c::s => SEQ(CHAR(c), charlist2rexp(s))
}

implicit def string2rexp(s: String): Rexp = charlist2rexp(s.toList)

implicit def RexpOps (r: Rexp) = new {
  def | (s: Rexp) = ALT(r, s)
  def % = STAR(r)
  def ~ (s: Rexp) = SEQ(r, s)
}

implicit def stringOps (s: String) = new {
  def | (r: Rexp) = ALT(s, r)
  def | (r: String) = ALT(s, r)
  def % = STAR(s)
  def ~ (r: Rexp) = SEQ(s, r)
  def ~ (r: String) = SEQ(s, r)
}

// nullable(r): test if regex matches the empty string
def nullable (r: Rexp): Boolean = r match {
  case ZERO => false
  case ONE => true
  case CHAR(_) => false
  case ALT(r1, r2) => nullable(r1) || nullable(r2)
  case SEQ(r1, r2) => nullable(r1) && nullable(r2)
  case STAR(_) => true
  case NTIMES(r, i) => if (i == 0) true else nullable(r)
}

// der(c, r): derivative of regex w.r.t. a character
// finds the regex that matches {s | c::s, s in L(r)}
def der (c: Char, r: Rexp): Rexp = r match {
  case ZERO => ZERO
  case ONE => ZERO
  case CHAR(d) => if (c == d) ONE else ZERO
  case ALT(r1, r2) => ALT(der(c, r1), der(c, r2))
  case SEQ(r1, r2) => {
    if (nullable(r1)) ALT(SEQ((der(c, r1)), r2), der(c, r2))
    else SEQ(der(c, r1), r2)
  }
  case STAR(r1) => SEQ(der(c, r1), STAR(r1))
  case NTIMES(r1, i) =>
    if (i == 0) ZERO else der(c, SEQ(r1, NTIMES(r1, i - 1)))
}

def simp(r: Rexp): Rexp = r match {
  case ALT(r1, r2) => (simp(r1), simp(r2)) match {
    case (ZERO, r2s) => r2s
    case (r1s, ZERO) => r1s
    case (r1s, r2s) => if (r1s == r2s) r1s else ALT(r1s, r2s)
  }
  case SEQ(r1, r2) => (simp(r1), simp(r2)) match {
    case (ZERO, _) => ZERO
    case (_, ZERO) => ZERO
    case (ONE, r2s) => r2s
    case (r1s, ONE) => r1s
    case (r1s, r2s) => SEQ(r1s, r2s)
  }
  case r => r
}

def ders (s: List[Char], r: Rexp): Rexp = s match {
  case Nil => r
  case c::cs => ders(cs, simp(der(c, r)))
}

def matcher(r: Rexp, s: String): Boolean = nullable(ders(s.toList, r))

def replace(r: Rexp, s1: String, s2: String): String = {
  if (!s1.isEmpty){
    val test = (for (j <- (1 to s1.length).reverse; if (matcher(r, s1.substring(0, j)))) yield s1.substring(0, j))

    if (!test.isEmpty) {
      s2 + replace(r, s1.substring(test(0).length, s1.length), s2)
    }
    else {
      s1.substring(0, 1) + replace(r, s1.substring(1, s1.length), s2)
    }
  }
  else {
    ""
  }
}

val EVIL = SEQ(STAR(STAR(CHAR('a'))), CHAR('b'))
val TESTNTIMES = NTIMES(CHAR('a'), 5)
val TESTNTIMESZERO = NTIMES(CHAR('a'), 0)
println(matcher(EVIL, "a" * 1000 ++ "b"))
println(!matcher(EVIL, "a" * 1000))
println(!matcher(TESTNTIMES, ""))
println(matcher(TESTNTIMES, "a" * 5))
println(!matcher(TESTNTIMES, "a" * 6))
println(matcher(TESTNTIMESZERO, ""))
println(!matcher(TESTNTIMESZERO, "a" * 10))

def time_needed[T](i: Int, code: => T) = {
  val start = System.nanoTime()
  for (j <- 1 to i) code
  val end = System.nanoTime()
  (end - start)/(i * 1.0e9)
}

for (i <- 1 to 5000001 by 500000) {
  println(i + " " + "%.5f".format(time_needed(2, matcher(EVIL, "a" * i))))
}


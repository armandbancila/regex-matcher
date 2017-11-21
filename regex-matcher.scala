abstract class Rexp
// basic regular expressions
case object ZERO extends Rexp
case object ONE extends Rexp
case class CHAR(c: Char) extends Rexp
case class ALT(r1: Rexp, r2: Rexp) extends Rexp
case class SEQ(r1: Rexp, r2: Rexp) extends Rexp
case class STAR(r: Rexp) extends Rexp
// extended regular expressions
case class CHARSET(x: Set[Char]) extends Rexp
case class PLUS(r: Rexp) extends Rexp
case class OPT(r: Rexp) extends Rexp
case class NTIMES(r: Rexp, n: Int) extends Rexp
case class UPTO(r: Rexp, n: Int) extends Rexp
case class ATLEAST(r: Rexp, n: Int) extends Rexp
case class BETWEEN(r: Rexp, n: Int, m: Int) extends Rexp
case class NOT(r: Rexp) extends Rexp
case class CFUN(f: Char => Boolean) extends Rexp
case object ALL extends Rexp

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

// test if regex matches the empty string
def nullable (r: Rexp): Boolean = r match {
  case ZERO => false
  case ONE => true
  case CHAR(_) => false
  case ALT(r1, r2) => nullable(r1) || nullable(r2)
  case SEQ(r1, r2) => nullable(r1) && nullable(r2)
  case STAR(_) => true
  case CHARSET(_) => false
  case PLUS(r) => nullable(r)
  case OPT(_) => true
  case NTIMES(r, n) => if (n == 0) true else nullable(r)
  case UPTO(_, _) => true
  case ATLEAST(r, n) => if (n == 0) true else nullable(r)
  case BETWEEN(r, n, m) => if (n == 0) true else nullable(r)
  case NOT(r) => !nullable(r)
  case CFUN(f) => false
  case ALL => false
}

// derivative of a regex w.r.t. a character
// finds the regex that matches {s | c::s, s in L(r)}
def der (c: Char, r: Rexp): Rexp = r match {
  case ZERO => ZERO
  case ONE => ZERO
  case CHAR(d) => der(c, CFUN(Set(d)))
  case ALT(r1, r2) => ALT(der(c, r1), der(c, r2))
  case SEQ(r1, r2) => {
    if (nullable(r1)) ALT(SEQ((der(c, r1)), r2), der(c, r2))
    else SEQ(der(c, r1), r2)
  }
  case STAR(r) => SEQ(der(c, r), STAR(r))
  case CHARSET(x) => der(c, CFUN(x))
  case PLUS(r) => SEQ(der(c, r), STAR(r))
  case OPT(r) => der(c, r)
  case NTIMES(r, n) => (r, n) match {
    case (r, 0) => ZERO
    case (r, 1) => der(c, r)
    case (r, n) => SEQ(der(c, r), NTIMES(r, n - 1))
  }
  case UPTO(r, n) => (r, n) match {
    case (r, 0) => ZERO
    case (r, 1) => der(c, r)
    case (r, n) => SEQ(der(c, r), UPTO(r, n - 1))
  }
  case ATLEAST(r, n) => SEQ(der(c, r), ATLEAST(r, n - 1))
  case BETWEEN(r, n, m) => (r, n, m) match {
    case (_, _, 0) => ZERO
    case (_, _, 1) => der(c, r)
    case (r, 0, m) => SEQ(der(c, r), BETWEEN(r, 0, m - 1))
    case (r, n, m) => SEQ(der(c, r), BETWEEN(r, n - 1, m - 1))
  }
  case NOT(r) => NOT(der(c, r))
  case CFUN(f) => if (f(c)) ONE else ZERO
  case ALL => der(c, CFUN((x: Char) => true))
}

// simplification rules for regexes
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

// derivative of a regex w.r.t. a string
def ders (s: List[Char], r: Rexp): Rexp = s match {
  case Nil => r
  case c::cs => ders(cs, simp(der(c, r)))
}

// faster than ders in most cases
def ders2(s: List[Char], r: Rexp): Rexp = (s, r) match {
  case (Nil, r) => r
  case (s, ZERO) => ZERO
  case (s, ONE) => if (s == Nil) ONE else ZERO
  case (s, CHAR(c)) => if (s == List(c)) ONE else 
                       if (s == Nil) CHAR(c) else ZERO
  case (s, ALT(r1, r2)) => ALT(ders2(s, r1), ders2(s, r2))
  case (c::s, r) => ders2(s, simp(der(c, r)))
}

// a regex matches a string <=> ders(string, regex) matches the empty string
def matcher(r: Rexp, s: String): Boolean = nullable(ders2(s.toList, r))

// replace matched string with a different string
// uses recursive, naive string search
def replace(r: Rexp, text: String, word: String): String = {
  if (!text.isEmpty) {
    // check if the leftmost part of the string is matched by regex
    // first check entire text, then drop rightmost chars one by one
    val matches = (for (j <- 0 to text.length; if (matcher(r, text.dropRight(j)))) yield text.dropRight(j))
    // if match found, replace match and check the dropped, rightmost part too
    // else drop leftmost char and check again
    if (!matches.isEmpty) word + replace(r, text.substring(matches.head.length, text.length), word)
    else text.head + replace(r, text.tail, word)
  }
  else ""
}

// examples
// the evil regex
val EVIL = SEQ(STAR(STAR(CHAR('a'))), CHAR('b'))
println(matcher(EVIL, "a" * 1000 ++ "b"))

// email regex, find and replace
val domainBeginCharset = (('a' to 'z') ++ ('0' to '9')).toSet + '.' + '-'
val domainEndCharset = ('a' to 'z').toSet + '.'
val localPartCharset = domainBeginCharset + '_'
val LOCALPART = PLUS(CHARSET(localPartCharset))
val DOMAINBEGIN = PLUS(CHARSET(domainBeginCharset))
val DOMAINEND = BETWEEN(CHARSET(domainEndCharset), 2, 6)
val DOMAIN = DOMAINBEGIN ~ CHAR('.') ~ DOMAINEND
val EMAIL = LOCALPART ~ CHAR('@') ~ DOMAIN

// is the string an email?
println(matcher(EMAIL, "alice@gmail.com"))
// find the pattern "alice" and replace it in the email with "bob"
println(replace("alice", "123.alice@gmail.com", "bob"))
// final derivative of the email, before it's tested with nullable
println(ders2("john@gmail.com".toList, EMAIL))


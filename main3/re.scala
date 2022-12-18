// Main Part 3 about Regular Expression Matching
//=============================================

object M3 {

// Regular Expressions
abstract class Rexp
case object ZERO extends Rexp
case object ONE extends Rexp
case class CHAR(c: Char) extends Rexp
case class ALTs(rs: List[Rexp]) extends Rexp      // alternatives 
case class SEQ(r1: Rexp, r2: Rexp) extends Rexp   // sequence
case class STAR(r: Rexp) extends Rexp             // star

// some convenience for typing regular expressions

//the usual binary choice can be defined in terms of ALTs
def ALT(r1: Rexp, r2: Rexp) = ALTs(List(r1, r2))

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

// (1) Complete the function nullable according to
// the definition given in the coursework; this 
// function checks whether a regular expression
// can match the empty string and Returns a boolean
// accordingly.

def nullable (r: Rexp) : Boolean = r match {

   case ZERO => false
   case ONE => true
   case CHAR(_) => false
   case ALTs(List(x,y)) => nullable(x) || nullable(y)
   case SEQ(x,y) => nullable(x) && nullable(y)
   case STAR(_) => true

}


// (2) Complete the function der according to
// the definition given in the coursework; this
// function calculates the derivative of a 
// regular expression w.r.t. a character.

def der (c: Char, r: Rexp) : Rexp = r match {

  case ZERO => ZERO
  case ONE => ZERO
  case CHAR(d) =>{
                    if(c == d){ONE}
                    else{ZERO}      
                  } 
  case ALTs(List(x,y)) => ALT(der(c,x),der(c,y))
  case SEQ(x,y) => {
                      if(nullable(x)){ALT(SEQ(der(c,x),y),der(c,y))}
                      else{SEQ(der(c,x),y)}
                    } 
  case STAR(x) => SEQ(der(c,x),STAR(x))
  
}


// (3) Implement the flatten function flts. It
// deletes 0s from a list of regular expressions
// and also 'spills out', or flattens, nested 
// ALTernativeS.

def flts(rs: List[Rexp]) : List[Rexp] = rs match {
     case List() => List()
     case ZERO :: s => flts(s)
     case ALTs(x) :: s => x ::: flts(s)
     case x :: s => x :: flts(s)
}

// (4) Complete the simp function according to
// the specification given in the coursework description; 
// this function simplifies a regular expression from
// the inside out, like you would simplify arithmetic 
// expressions; however it does not simplify inside 
// STAR-regular expressions. Use the _.distinct and 
// flts functions.

def simp(r: Rexp) : Rexp =  r match {
  case SEQ(a,b) => (simp(a),simp(b)) match {
    case (_, ZERO) => ZERO
    case (ZERO, _) => ZERO
    case (x, ONE) => x
    case (ONE, x) => x
    case (x,y) => SEQ(x,y)
  }

  case ALTs(List(a,b)) => (flts(List(simp(a),simp(b)))).distinct match {
    case List() => ZERO
    case List(r) => r
    case _ => r
  }

  case _ => r
}

//testcases for simp
// simp(ZERO | ONE) == ONE
// simp(STAR(ZERO | ONE)) == STAR(ZERO | ONE)
// simp(ONE ~ (ONE ~ (ONE ~ CHAR('a')))) == CHAR('a')
// simp(((ONE ~ ONE) ~ ONE) ~ CHAR('a')) == CHAR('a')
// simp(((ONE | ONE) ~ ONE) ~ CHAR('a')) == CHAR('a')
// simp(ONE ~ (ONE ~ (ONE ~ ZERO))) == ZERO
// simp(ALT(ONE ~ (ONE ~ (ONE ~ ZERO)), CHAR('a'))) == CHAR('a')
// simp(CHAR('a') | CHAR('a')) == CHAR('a')
// simp(CHAR('a') ~ CHAR('a')) == CHAR('a') ~ CHAR('a')
// simp(ONE | CHAR('a')) == (ONE | CHAR('a'))
// simp(ALT((CHAR('a') | ZERO) ~ ONE,
// ((ONE | CHAR('b')) | CHAR('c')) ~ (CHAR('d') ~ ZERO))) == CHAR('a')
// simp((ZERO | ((ZERO | ZERO) | (ZERO | ZERO))) ~ ((ONE | ZERO) | ONE ) ~ (CHAR('a'))) == ZERO
// simp(ALT(ONE | ONE, ONE | ONE)) == ONE
// simp(ALT(ZERO | CHAR('a'), CHAR('a') | ZERO)) == CHAR('a')

// simp(ALTs(List(ONE | CHAR('a'), CHAR('a') | ONE))) == ALTs(List(ONE, CHAR('a')))


// (5) Complete the two functions below; the first 
// calculates the derivative w.r.t. a string; the second
// is the regular expression matcher taking a regular
// expression and a string and checks whether the
// string matches the regular expression

def ders (s: List[Char], r: Rexp) : Rexp = s match {
  
  case Nil => r
  case c::cs => ders(cs,simp(der(c,r)))

}

def matcher(r: Rexp, s: String): Boolean ={
//It builds first the derivatives according to ders and after
// that tests whether the resulting derivative regular expression can match
// the empty string (using nullable).
  nullable(ders(s.toList,r))
}

// (6) Complete the size function for regular
// expressions according to the specification 
// given in the coursework.

def size(r: Rexp): Int =  r match {
  case ZERO => 1
  case ONE => 1
  case CHAR(_) => 1
  case ALTs(List(x, y)) => 1 + size(x) + size(y)
  case SEQ(x, y) => 1 + size(x) + size(y)
  case STAR(x) => 1 + size(x)
}

// some testing data

/*
matcher(("a" ~ "b") ~ "c", "abc")  // => true
matcher(("a" ~ "b") ~ "c", "ab")   // => false

// the supposedly 'evil' regular expression (a*)* b
val EVIL = SEQ(STAR(STAR(CHAR('a'))), CHAR('b'))

matcher(EVIL, "a" * 1000 ++ "b")   // => true
matcher(EVIL, "a" * 1000)          // => false

// size without simplifications
size(der('a', der('a', EVIL)))             // => 28
size(der('a', der('a', der('a', EVIL))))   // => 58

// size with simplification
size(simp(der('a', der('a', EVIL))))           // => 8
size(simp(der('a', der('a', der('a', EVIL))))) // => 8

// Python needs around 30 seconds for matching 28 a's with EVIL. 
// Java 9 and later increase this to an "astonishing" 40000 a's in
// 30 seconds.
//
// Lets see how long it really takes to match strings with 
// 5 Million a's...it should be in the range of a couple
// of seconds.

def time_needed[T](i: Int, code: => T) = {
  val start = System.nanoTime()
  for (j <- 1 to i) code
  val end = System.nanoTime()
  "%.5f".format((end - start)/(i * 1.0e9))
}

for (i <- 0 to 5000000 by 500000) {
  println(s"$i ${time_needed(2, matcher(EVIL, "a" * i))} secs.") 
}

// another "power" test case 
simp(Iterator.iterate(ONE:Rexp)(r => SEQ(r, ONE | ONE)).drop(50).next()) == ONE

// the Iterator produces the rexp
//
//      SEQ(SEQ(SEQ(..., ONE | ONE) , ONE | ONE), ONE | ONE)
//
//    where SEQ is nested 50 times.

*/

}

// Main Part 3 about Regular Expression Matching
//==============================================

object M3 {

abstract class Rexp
case object ZERO extends Rexp
case object ONE extends Rexp
case class CHAR(c: Char) extends Rexp
case class ALTs(rs: List[Rexp]) extends Rexp  // alternatives 
case class SEQs(rs: List[Rexp]) extends Rexp  // sequences
case class STAR(r: Rexp) extends Rexp         // star


//the usual binary choice and binary sequence can be defined 
//in terms of ALTs and SEQs
def ALT(r1: Rexp, r2: Rexp) = ALTs(List(r1, r2))
def SEQ(r1: Rexp, r2: Rexp) = SEQs(List(r1, r2))


// some convenience for typing regular expressions
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

// examples for the implicits:
// ALT(CHAR('a'), CHAR('b'))
// val areg : Rexp = "a" | "b"

// SEQ(CHAR('a'), CHAR('b')) 
// val sreg : Rexp = "a" ~ "b"


// ADD YOUR CODE BELOW
//======================

// (1)

  
def nullable (r: Rexp) : Boolean = r match {
  case ZERO => false
  case ONE  => true
  case CHAR(a) => false
  case ALTs( orig ) => orig.exists(nullable)
  case SEQs( orig ) => orig.forall(nullable)
  case STAR(a) => true
}

// (2) 
def der (c: Char, r: Rexp) : Rexp = r match {
  case ZERO => ZERO
  case ONE => ZERO
  case CHAR(d) if c == d => ONE
  case CHAR(d) if c != d => ZERO
  case ALTs( orig ) => ALTs(orig.map( x => der(c,x) ))
  case SEQs(Nil) => ZERO
  case SEQs(r::rs) if nullable(r) => ALT(SEQs(der(c,r)::rs),der(c,SEQs(rs)))
  case SEQs(r::rs) if !nullable(r) => SEQs(der(c,r)::rs)
  case STAR(r) => SEQ(der(c,r),STAR(r))
}

// (3) 
def denest(rs: List[Rexp]) : List[Rexp] = rs match {
  case Nil => rs
  case ZERO::rest => denest(rest)
  case ALTs(a)::rest => a:::denest(rest)
  case r::rest => r::denest(rest)
}

// (4)
def flts(rs: List[Rexp], acc: List[Rexp] = Nil) : List[Rexp] = rs match {
  case Nil => acc
  case ZERO::rest => List(ZERO)
  case ONE::rest => flts(rest,acc)
  case SEQs(a)::rest => flts(rest,acc:::a)
  case r::rest => flts(rest,acc:::List(r))
}

// (5)
def ALTs_smart(rs: List[Rexp]) : Rexp = rs match {
  case Nil => ZERO
  case List(r) => r
  case _ => ALTs(rs)
}
def SEQs_smart(rs: List[Rexp]) : Rexp = rs match {
  case Nil => ONE
  case List(ZERO) => ZERO
  case List(r) => r
  case _ => SEQs(rs)
}

// (6)
def simp(r: Rexp) : Rexp = r match {
  case ALTs(orig) => ALTs_smart( denest( orig.map( x => simp(x) ).toList ).distinct )
  case SEQs(orig) => SEQs_smart( flts( orig.map( x => simp(x) ).toList ) )
  case _ => r
}

// (7)
def ders (s: List[Char], r: Rexp) : Rexp = s match {
  case Nil => r
  case c::cs => ders(cs,simp(der(c,r)))
}
  
def matcher(r: Rexp, s: String): Boolean = nullable(ders(s.toList,r))

// (8) 
def size(r: Rexp): Int = r match {
  case ZERO => 1
  case ONE => 1
  case CHAR(a) => 1
  case ALTs( orig ) => 1 + orig.map( x => size(x) ).sum
  case SEQs( orig ) => 1 + orig.map( x => size(x) ).sum
  case STAR(a) => 1 + size(a)
}

def MyTest() : Unit = {
  println( simp(ALT(ONE | CHAR('a'), CHAR('a') | ONE))  );
  println( simp(ALT(ONE | CHAR('a'), CHAR('a') | ONE))  == ALTs(List(ONE, CHAR('a'))) );
  
  println( simp(((CHAR('a') | ZERO) ~ ONE) | (((ONE | CHAR('b')) | CHAR('c')) ~ (CHAR('d') ~ ZERO)))  );
  println( simp(((CHAR('a') | ZERO) ~ ONE) | (((ONE | CHAR('b')) | CHAR('c')) ~ (CHAR('d') ~ ZERO))) == CHAR('a') );
  
  println( matcher(("a" ~ "b") ~ "c", "ab") );  // => false
  println( matcher(("a" ~ "b") ~ "c", "abc") ); // => true
  
  
  // the supposedly 'evil' regular expression (a*)* b
  val EVIL = SEQ(STAR(STAR(CHAR('a'))), CHAR('b'));
  
  println( matcher(EVIL, "a" * 1000)  );        // => false
  println( matcher(EVIL, "a" * 1000 ++ "b")  ); // => true
  
  
  // size without simplifications
  print( size(der('a', der('a', EVIL))) );            // => 36
  print( size(der('a', der('a', der('a', EVIL)))) );  // => 83
  
  // size with simplification
  print( size(simp(der('a', der('a', EVIL))))     );      // => 7
  print( size(simp(der('a', der('a', der('a', EVIL)))))  );// => 7
}
}

// This template code is subject to copyright 
// by King's College London, 2022. Do not 
// make the template code public in any shape 
// or form, and do not exchange it with other 
// students under any circumstance

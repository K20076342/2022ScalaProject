object M2 { 

import io.Source
import scala.util._

// ADD YOUR CODE BELOW
//======================

  /*
val wordle_url = """https://nms.kcl.ac.uk/christian.urban/wordle.txt"""
val secrets = M2.get_wordle_list(wordle_url)
secrets.length == 12972
secrets.filter(_.length != 5) == Nil
M2.get_wordle_list(wordle_url ++ "2") == Nil
  */

//(1)
def get_wordle_list(url: String) : List[String] = {
  try{
    val raw = Source.fromURL( url ).mkString
    raw.split('\n').toList
  }
  catch{
    case _: Throwable =>  List[String]()
  }
  finally{
    List[String]()
  }
}

// val secrets = M2.get_wordle_list("https://nms.kcl.ac.uk/christian.urban/wordle.txt")
// secrets.length // => 12972
// secrets.filter(_.length != 5) // => Nil

//(2)
def removeN[A](xs: List[A], elem: A, n: Int) : List[A] = {
  if( n == 0 ) xs
  else{
    val idx = xs.indexOf( elem )
    if( idx == -1 ) xs
    else{
      
      val cur = xs.slice(0,idx).toList++xs.slice(idx+1,xs.size).toList;
      removeN( cur , elem , n-1 )
    }
  }
}


// removeN(List(1,2,3,2,1), 3, 1)  // => List(1, 2, 2, 1)
// removeN(List(1,2,3,2,1), 2, 1)  // => List(1, 3, 2, 1)
// removeN(List(1,2,3,2,1), 1, 1)  // => List(2, 3, 2, 1)
// removeN(List(1,2,3,2,1), 0, 2)  // => List(1, 2, 3, 2, 1)

// (3)
abstract class Tip
case object Absent extends Tip
case object Present extends Tip
case object Correct extends Tip


def pool(secret: String, word: String) : List[Char] = {

  if (secret.length ==1){
    if (secret.charAt(0)!= word.charAt(0)){
        List(secret.charAt(0))
      }else{
        List()}
      }
      else if
      (secret.charAt(0) != word.charAt(0)){
          List(secret.charAt(0)) ++ pool(secret.substring(1), word.substring(1))
      }
      else{
          List() ++ pool(secret.substring(1), word.substring(1))
        }
      }
  
  

def aux(secret: List[Char], word: List[Char], pool: List[Char]) : List[Tip] = {
  (secret,word) match {
    case (Nil, _) => Nil
    case (_ , Nil) => Nil
    case (x::xs, y::ys) =>{
      if (x==y) Correct :: aux(xs, ys, pool)
      else if(pool.contains(y)){
        val anotherPool= pool.diff(List(y))
        Present :: aux(xs, ys, anotherPool)
      }
      else Absent :: aux(xs, ys, pool)
    }
  }
}

def score(secret: String, word: String) : List[Tip] = {
  val pool1 = pool(secret, word)
  aux (secret.toList , word.toList , pool1)
}

/*
score("chess", "caves") == List(Correct, Absent, Absent, Present, Correct)
M2.score("doses", "slide") == List(Present, Absent, Absent, Present, Present)
M2.score("chess", "swiss") == List(Absent, Absent, Absent, Correct, Correct)
M2.score("chess", "eexss") == List(Present, Absent, Absent, Correct, Correct)
*/

// (4)

// def toPoint(a : Tip ) : Int = {
  
// }
def eval(t: Tip) : Int = {
  val chk = List(Correct , Present  , Absent )
  if( t.getClass.getName == chk(0).getClass.getName ) 10
  else  if( t.getClass.getName == chk(1).getClass.getName ) 1
  else 0
}

def iscore(secret: String, word: String) : Int = {
  val raw = score(secret,word)
  val res = raw.map( x => eval(x) ).toList
  res.sum
}

//iscore("chess", "caves") // => 21
//iscore("chess", "swiss") // => 20

// (5)
def lowest(secrets: List[String], word: String, current: Int, acc: List[String]) : List[String] = {
  secrets match {
    case Nil => acc
    case x :: xs => {
      val score1 = iscore(x, word)
      if (score1 == current) lowest (xs, word, current, acc:+ x)
      else if (score1 < current) lowest (xs, word, score1, List(x))
      else lowest (xs, word, current, acc)
    }
  }
}
  
def evil(secrets: List[String], word: String) : List[String] = {
  val mn = secrets.map( s => iscore(s,word) ).toList.min
  val res = secrets.filter( x => iscore(x,word)==mn )
  res
}


//evil(secrets, "stent").length
//evil(secrets, "hexes").length
//evil(secrets, "horse").length
//evil(secrets, "hoise").length
//evil(secrets, "house").length

// (6)

def countChar( secrets: List[String] , c : Char ) : Int = {
  val l = secrets.map( x => x.count(_ == c) ).toList
  l.sum
}

def countTotal( secrets: List[String] ) : Int = {
  val l = secrets.map( x => x.size ).toList
  l.sum
}
def frequencies(secrets: List[String]) : Map[Char, Double] = {

  val n = countTotal( secrets ).toDouble
  val cnt = ( 'a' to 'z' ).map( c => countChar( secrets , c) ).toList
  val freq = cnt.map( x => 1.0 - x.toDouble / n ).toList
  val AtoZ = ( 'a' to 'z' ).toList
  val res = ( AtoZ zip freq ).toMap
  res
}

// (7)
def rank(frqs: Map[Char, Double], s: String) : Double = {
  val l = s.map( c => frqs.get(c).getOrElse(0.0) ).toList
  l.sum
}

def ranked_evil(secrets: List[String], word: String) : List[String] = {
  val l_mn_score = evil(secrets , word )
  val frqs = frequencies(secrets)
  val l = l_mn_score.map( x => (rank(frqs,x),x) ).toList
  val mx_val = l.maxBy( _._1 )._1
  //val mx_val = l.minBy( _._1 )._1

  val res = l.filter( x => x._1 == mx_val )
  res.map( x => x._2 ).toList
}

}



// This template code is subject to copyright 
// by King's College London, 2022. Do not 
// make the template code public in any shape 
// or form, and do not exchange it with other 
// students under any circumstance.

/*
M2.evil(secrets, "stent").length == 1907
M2.evil(secrets, "hexes").length == 2966
M2.evil(secrets, "horse").length == 1203
M2.evil(secrets, "hoise").length == 971
M2.evil(secrets, "house").length == 1228
*/

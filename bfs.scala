object M5b {

// !!! Copy any function you need from file bf.scala !!!
//
// If you need any auxiliary function, feel free to 
// implement it, but do not make any changes to the
// templates below.


// DEBUGGING INFORMATION FOR COMPILERS!!!
//
// Compiler, even real ones, are fiendishly difficult to get
// to produce correct code. One way to debug them is to run
// example programs ``unoptimised''; and then optimised. Does
// the optimised version still produce the same result?


// for timing purposes
def time_needed[T](n: Int, code: => T) = {
  val start = System.nanoTime()
  for (i <- 0 until n) code
  val end = System.nanoTime()
  (end - start)/(n * 1.0e9)
}


type Mem = Map[Int, Int]

import io.Source
import scala.util._

// ADD YOUR CODE BELOW
//======================
// (1)
def load_bff(name: String) : String = {
  try Source.fromFile(name).mkString
  catch {
    case _: Throwable => ""
  }
}

// (2) 

def sread(mem: Mem, mp: Int) : Int = {
  mem.get(mp).getOrElse(0)
}

def write(mem: Mem, mp: Int, v: Int) : Mem = {
  val newMem = mem + (mp -> v);
  newMem
}

// (3) 

def jumpRight(prog: String, pc: Int, level: Int) : Int = {
  if( pc >= 0 && pc < prog.size ){
    val cur = prog(pc);
    cur match {
      case '[' if level >= 0 => jumpRight(prog, pc+1, level+1)
      case ']' if level >= 1 => jumpRight(prog, pc+1, level-1)
      case ']' if level == 0 => pc+1
      case _ => jumpRight(prog, pc+1, level)
    }
  }
  else{
    pc
  }
}

def jumpLeft(prog: String, pc: Int, level: Int) : Int = {
  if( pc >= 0 && pc < prog.size ){
    val cur = prog(pc);
    cur match {
      case ']' if level >= 0 => jumpLeft(prog, pc-1, level+1)
      case '[' if level >= 1 => jumpLeft(prog, pc-1, level-1)
      case '[' if level == 0 => pc+1
      case _ => jumpLeft(prog, pc - 1, level)
    }
  }
  else{
    pc
  }
}
// (4) 

def compute(prog: String, pc: Int, mp: Int, mem: Mem) : Mem = {
  if( pc >= 0 && pc < prog.size ){
    val cur = prog(pc);
    cur match {
      case '>' => compute(prog, pc+1, mp+1, mem)
      case '<' => compute(prog, pc+1, mp-1, mem)
      case '+' => compute(prog, pc+1, mp, write(mem, mp, sread(mem, mp)+1))
      case '-' => compute(prog, pc+1, mp, write(mem, mp, sread(mem, mp)-1))
      case '.' => {
        print(sread(mem,mp).toChar)
        compute(prog, pc+1, mp, mem)
      }
      case ',' => compute(prog, pc+1, mp, write(mem, mp, Console.in.read().toByte))
      case '[' if sread(mem,mp) == 0 => compute(prog, jumpRight(prog, pc+1, 0), mp, mem)
      case ']' if sread(mem,mp) != 0 => compute(prog, jumpLeft(prog, pc-1, 0), mp, mem)
      case _ => compute(prog, pc+1, mp, mem)
    }
  }
  else{
    mem
  }
}

def run(prog: String, m: Mem = Map()) = compute(prog, 0, 0, m)

// (5)
def generate(msg: List[Char]) : String = msg.toString


// (6) 
def jtable(pg: String) : Map[Int, Int] = {
  (0 to pg.size-1).toList.filter( i => pg(i) == '[' ).map( x => x -> jumpRight(pg, x+1,0) ).map( e => List(e, (e._2-1)->(e._1+1) ) ).flatten.toMap
}

// testcase
//
// jtable("""+++++[->++++++++++<]>--<+++[->>++++++++++<<]>>++<<----------[+>.>.<+<]""")
// =>  Map(69 -> 61, 5 -> 20, 60 -> 70, 27 -> 44, 43 -> 28, 19 -> 6)


def compute2(pg: String, tb: Map[Int, Int], pc: Int, mp: Int, mem: Mem) : Mem = {
  if( pc >= 0 && pc < pg.size ){
    val cur = pg(pc);
    cur match {
      case '>' => compute2(pg, tb, pc+1, mp+1, mem)
      case '<' => compute2(pg, tb, pc+1, mp-1, mem)
      case '+' => compute2(pg, tb, pc+1, mp, write(mem, mp, sread(mem, mp)+1))
      case '-' => compute2(pg, tb, pc+1, mp, write(mem, mp, sread(mem, mp)-1))
      case '.' =>
        print(sread(mem,mp).toChar)
        compute2(pg, tb, pc+1, mp, mem)
      case ',' => compute2(pg, tb, pc+1, mp, write(mem, mp, Console.in.read().toByte))
      case '[' if sread(mem, mp) == 0 => compute2(pg, tb, tb(pc), mp, mem)
      case ']' if sread(mem, mp) != 0 => compute2(pg, tb, tb(pc), mp, mem)
      case _ => compute2(pg, tb, pc+1, mp, mem)
    }
  }
  else{
    mem
  }
}


  
def run2(pg: String, m: Mem = Map()) = compute2(pg, jtable(pg), 0, 0, m)

// testcases
// time_needed(1, run2(load_bff("benchmark.bf")))
// time_needed(1, run2(load_bff("sierpinski.bf")))



// (7) 

def optimise(s: String) : String = {
  s.replaceAll("""[^<>+-.,\[\]]""","").replaceAll("""\[-\]""","0")
}

def compute3(pg: String, tb: Map[Int, Int], pc: Int, mp: Int, mem: Mem) : Mem = {
  if( pc >= 0 && pc < pg.size ){
    val cur = pg(pc);
    cur match {
      case '>' => compute3(pg, tb, pc+1, mp+1, mem)
      case '<' => compute3(pg, tb, pc+1, mp-1, mem)
      case '+' => compute3(pg, tb, pc+1, mp, write(mem, mp, sread(mem, mp)+1))
      case '-' => compute3(pg, tb, pc+1, mp, write(mem, mp, sread(mem, mp)-1))
      case '.' =>
        print(sread(mem,mp).toChar)
        compute3(pg, tb, pc+1, mp, mem)
      case ',' => compute3(pg, tb, pc+1, mp, write(mem, mp, Console.in.read().toByte))
      case '[' if sread(mem, mp) == 0 => compute3(pg, tb, tb(pc), mp, mem)
      case ']' if sread(mem, mp) != 0 => compute3(pg, tb, tb(pc), mp, mem)
      case '0' => compute3(pg, tb, pc+1, mp, write(mem, mp, 0))
      case _ => compute3(pg, tb, pc+1, mp, mem)
    }
  }
  else{
    mem
  }
}

def run3(pg: String, m: Mem = Map()) = compute3(optimise(pg), jtable(optimise(pg)), 0, 0, m)


// testcases
//
// optimise(load_bff("benchmark.bf"))          // should have inserted 0's
// optimise(load_bff("mandelbrot.bf")).length  // => 11203
// 
// time_needed(1, run3(load_bff("benchmark.bf")))



// (8)  
def combine(s: String) : String = {
 val str2 = s.replaceAll("(.)\\1+", "+A")
  val str3 = str2.replaceAll("(.)\\1+", "+B")
  val str4 = str3.replaceAll("(.)\\1+", "+C")
  val str5 = str4.replaceAll("(.)\\1+", "+D")
  val str6 = str5.replaceAll("(.)\\1+", "+E")
  val str7 = str6.replaceAll("(.)\\1+", "+F")
  val str8 = str7.replaceAll("(.)\\1+", "+G")
  val str9 = str8.replaceAll("(.)\\1+", "+H")
  val str10 = str9.replaceAll("(.)\\1+", "+I")
  val str11 = str10.replaceAll("(.)\\1+", "+J")
  val str12 = str11.replaceAll("(.)\\1+", "+K")
  val str13 = str12.replaceAll("(.)\\1+", "+L")
  val str14 = str13.replaceAll("(.)\\1+", "+M")
  val str15 = str14.replaceAll("(.)\\1+", "+N")
  val str16 = str15.replaceAll("(.)\\1+", "+O")
  val str17 = str16.replaceAll("(.)\\1+", "+P")
  val str18 = str17.replaceAll("(.)\\1+", "+Q")
  val str19 = str18.replaceAll("(.)\\1+", "+R")
  val str20 = str19.replaceAll("(.)\\1+", "+S")
  val str21 = str20.replaceAll("(.)\\1+", "+T")
  val str22 = str21.replaceAll("(.)\\1+", "+U")
  val str23 = str22.replaceAll("(.)\\1+", "+V")
  val str24 = str23.replaceAll("(.)\\1+", "+W")
  val str25 = str24.replaceAll("(.)\\1+", "+X")
  val str26 = str25.replaceAll("(.)\\1+", "+Y")
  val str27 = str26.replaceAll("(.)\\1+", "+Z")

  str2 + str3 + str4 + str5 + str6 + str7 + str8 + str9 + str10 + str11 + str12 + str13 + str14 + str15 + str16 + str17 + str18 + str19 + str20 + str21 + str22 + str23 + str24 + str25 + str26 + str27
}


// testcase
// combine(load_bff("benchmark.bf"))

def compute4(pg: String, tb: Map[Int, Int], pc: Int, mp: Int, mem: Mem) : Mem = {
  if( pc >= 0 && pc < pg.size ){
    val cur = pg(pc);
    cur match {
      case '>' => compute4(pg, tb, pc+1, mp+1, mem)
      case '<' => compute4(pg, tb, pc+1, mp-1, mem)
      case '+' => compute4(pg, tb, pc+1, mp, write(mem, mp, sread(mem, mp)+1))
      case '-' => compute4(pg, tb, pc+1, mp, write(mem, mp, sread(mem, mp)-1))
      case '.' =>
        print(sread(mem,mp).toChar)
        compute4(pg, tb, pc+1, mp, mem)
      case ',' => compute4(pg, tb, pc+1, mp, write(mem, mp, Console.in.read().toByte))
      case '[' if sread(mem, mp) == 0 => compute4(pg, tb, tb(pc), mp, mem)
      case ']' if sread(mem, mp) != 0 => compute4(pg, tb, tb(pc), mp, mem)
      case '0' => compute4(pg, tb, pc+1, mp, write(mem, mp, 0))
      case _ => compute4(pg, tb, pc+1, mp, mem)
    }
  }
  else{
    mem
  }
}

// should call first optimise and then combine on the input string
//
def run4(pg: String, m: Mem = Map()) = {
   compute4(optimise(pg), jtable(optimise(pg)), 0, 0, m)
}


// testcases
// combine(optimise(load_bff("benchmark.bf"))) // => """>A+B[<A+M>A-A]<A[[....."""

// testcases (they should now run much faster)
// time_needed(1, run4(load_bff("benchmark.bf")))
// time_needed(1, run4(load_bff("sierpinski.bf"))) 
// time_needed(1, run4(load_bff("mandelbrot.bf")))


}




/*
             
scalac bfc.scala -d b1.jar && scala -cp b1.jar
             
*/

object C3a {

  // def main(args: Array[String]): Unit = {
  //   println("Main Function test");
  //   val l1 = split("3 + 4 * ( 2 - 1 )");
  //   // l1.map(x => println(x))

  //   val l2 = split("3 + 4 * 5 + 6");
  //   val r2 = syard(l2) // 3 4 5 *+ 6+

  //   Plt(syard(split("3 + 4 * ( 2 - 1 )"))) // 3 4 2 1 - * +
  //   Plt(syard(split("10 + 12 * 33")) ) // 10 12 33 * +
  //   Plt(syard(split("( 5 + 7 ) * 2")) ) // 5 7 + 2 *
  //   Plt(syard(split("5 + 7 / 2")) )// 5 7 2 / +
  //   Plt(syard(split("5 * 7 / 2")) )// 5 7 * 2 /
  //   Plt(syard(split("9 + 24 / ( 7 - 3 )")) ); // 9 24 7 3 - / +

  //   // test cases
  //   println(compute(syard(split("3 + 4 * ( 2 - 1 )")))) // 7
  //   println(compute(syard(split("10 + 12 * 33")))) // 406
  //   println(compute(syard(split("( 5 + 7 ) * 2")))) // 24
  //   println(compute(syard(split("5 + 7 / 2")))) // 8
  //   println(compute(syard(split("5 * 7 / 2")))) // 17
  //   println(compute(syard(split("9 + 24 / ( 7 - 3 )")))) // 15
  // // }
// type of tokens
  type Toks = List[String];

// the operations in the basic version of the algorithm
  val ops = List("+", "-", "*", "/");

// the precedences of the operators
  val precs = Map("+" -> 1, "-" -> 1, "*" -> 2, "/" -> 2)

// helper function for splitting strings into tokens
  def split(s: String): Toks = s.split(" ").toList;

// ADD YOUR CODE BELOW
//======================

// (1)
  def is_op(op: String): Boolean = {
    ops.contains(op)
  }
  def prec(op1: String, op2: String): Boolean = {
    // print("op1 : "); print(op1); print(" , op2 : "); print(op2);
    // println(precs.get(op1).getOrElse(0) <= precs.get(op2).getOrElse(0))
    precs.get(op1).getOrElse(0) <= precs.get(op2).getOrElse(0)
  }

  // def p(s: String): Unit = {
  //   print(s)
  //   print(" ");
  // }
  // def Plt(l: Toks): Unit = {
  //   if (l.size > 0) {
  //     l.map(x => p(x));
  //     println();
  //   } else {
  //     println("emt");
  //   }
  // }

  def syard(toks: Toks, st: Toks = Nil, out: Toks = Nil): Toks = {
    if (toks.size == 0) {
      (out ++ st.reverse).filter( _ != "(" )
    } else if (!is_op(toks(0)) && toks(0) != "(" && toks(0) != ")") {
      syard(toks.takeRight(toks.size - 1), st, out :+ toks.head)
    } else if (toks(0) == "(") {
      syard(toks.takeRight(toks.size - 1), st :+ toks.head, out);
    } else if (toks(0) == ")") {
      val idx = st.indexOf("(");
      val res = st.slice(idx + 1, st.size).reverse;
      syard(toks.takeRight(toks.size - 1), st.slice(0, idx), out ++ res)
    } else {
      if (st.size > 0 && prec(toks(0), st.last)) {
        syard(toks, st.slice(0, st.size - 1), out :+ st.last)
      } else syard(toks.takeRight(toks.size - 1), st :+ toks.head, out)
    }
  }

// test cases
// syard(split("3 + 4 * ( 2 - 1 )"))  // 3 4 2 1 - * +
// syard(split("10 + 12 * 33"))       // 10 12 33 * +
// syard(split("( 5 + 7 ) * 2"))      // 5 7 + 2 *
// syard(split("5 + 7 / 2"))          // 5 7 2 / +
// syard(split("5 * 7 / 2"))          // 5 7 * 2 /
// syard(split("9 + 24 / ( 7 - 3 )")) // 9 24 7 3 - / +

//syard(split("3 + 4 + 5"))           // 3 4 + 5 +
//syard(split("( ( 3 + 4 ) + 5 )"))    // 3 4 + 5 +
//syard(split("( 3 + ( 4 + 5 ) )"))    // 3 4 5 + +
//syard(split("( ( ( 3 ) ) + ( ( 4 + ( 5 ) ) ) )")) // 3 4 5 + +

// (2)
  def cal(n1: Int, n2: Int, op: String): Int = {
    if (op == "+") n1 + n2
    else if (op == "-") n1 - n2
    else if (op == "*") n1 * n2
    else n1 / n2
    // (op == "/")
  }
  def compute(toks: Toks, st: List[Int] = Nil): Int = {
    // println("----")
    // Plt(toks);
    // st.map( x => println(x) );
    if (toks.size == 0) {
      st.sum
    }
    else if (is_op(toks(0))) {
      val stSz = st.size;
      val b = st(stSz - 1);
      val a = st(stSz - 2);
      val c = cal(a, b, toks(0));
      compute(toks.takeRight(toks.size - 1), st.slice(0, stSz - 2):+ c);
    }
    else compute(toks.takeRight(toks.size - 1), st:+toks.head.toInt);
  }

// test cases
// compute(syard(split("3 + 4 * ( 2 - 1 )")))  // 7
// compute(syard(split("10 + 12 * 33")))       // 406
// compute(syard(split("( 5 + 7 ) * 2")))      // 24
// compute(syard(split("5 + 7 / 2")))          // 8
// compute(syard(split("5 * 7 / 2")))          // 17
// compute(syard(split("9 + 24 / ( 7 - 3 )"))) // 15

}

// This template code is subject to copyright
// by King's College London, 2022. Do not
// make the template code public in any shape
// or form, and do not exchange it with other
// students under any circumstance.

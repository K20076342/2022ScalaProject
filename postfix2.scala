// Shunting Yard Algorithm
// including Associativity for Operators
// =====================================

object C3b {

  // def main(args: List[String]): Unit = {
  //   println("main test");
  //   syard(split("3 + 4 * 8 / ( 5 - 1 ) ^ 2 ^ 3")).map(x => print(s"$x "));
  //   println();
  // }
// type of tokens
  type Toks = List[String]

// helper function for splitting strings into tokens
  def split(s: String): Toks = s.split(" ").toList

// left- and right-associativity
  abstract class Assoc
  case object LA extends Assoc
  case object RA extends Assoc

// power is right-associative,
// everything else is left-associative
  def assoc(s: String): Assoc = s match {
    case "^" => RA
    case _   => LA
  }

// the precedences of the operators
  val precs = Map("+" -> 1, "-" -> 1, "*" -> 2, "/" -> 2, "^" -> 4)
// the operations in the basic version of the algorithm
  val ops = List("+", "-", "*", "/", "^")

// ADD YOUR CODE BELOW
//======================
  def is_op(op: String): Boolean = {
    ops.contains(op)
  }

  def prec(op1: String, op2: String): Boolean = {
    precs.get(op1).getOrElse(0) <= precs.get(op2).getOrElse(0)
  }
// (3)
  def syard(toks: Toks, st: Toks = Nil, out: Toks = Nil): Toks = {
    if (toks.size == 0) {
      (out ++ st.reverse).filter(_ != "(")
    } else if (toks(0) == "(") {
      syard(toks.takeRight(toks.size - 1), st :+ toks.head, out);
    } else if (toks(0) == "^") {
      syard(toks.takeRight(toks.size - 1), st :+ toks.head, out);
    } else if (toks(0) == ")") {
      val idx = st.indexOf("(");
      val res = st.slice(idx + 1, st.size).reverse;
      syard(toks.takeRight(toks.size - 1), st.slice(0, idx), out ++ res)
    } else if (!is_op(toks(0)) && toks(0) != "(" && toks(0) != ")") {
      syard(toks.takeRight(toks.size - 1), st, out :+ toks.head)
    } else {
      if (st.size > 0 && prec(toks(0), st.last)) {
        syard(toks, st.slice(0, st.size - 1), out :+ st.last)
      } else syard(toks.takeRight(toks.size - 1), st :+ toks.head, out)
    }
  }

// test cases
// syard(split("3 + 4 * 8 / ( 5 - 1 ) ^ 2 ^ 3"))  // 3 4 8 * 5 1 - 2 3 ^ ^ / +

  def pow(x: Int, n: Int): Int = {
    if (n == 0) {
      1
    } else if (n % 2 == 0) {
      val tmp = pow(x, n / 2);
      tmp * tmp
    } else {
      x * pow(x, n - 1)
    }
  }

  def cal(n1: Int, n2: Int, op: String): Int = {
    if (op == "+") n1 + n2
    else if (op == "-") n1 - n2
    else if (op == "*") n1 * n2
    else if (op == "/") n1 / n2
    else pow(n1, n2)
    // (op == "/")
  }
// (4)
  def compute(toks: Toks, st: List[Int] = Nil): Int = {
    if (toks.size == 0) {
      st.sum
    } else if (is_op(toks(0))) {
      val stSz = st.size;
      val b = st(stSz - 1);
      val a = st(stSz - 2);
      val c = cal(a, b, toks(0));
      compute(toks.takeRight(toks.size - 1), st.slice(0, stSz - 2) :+ c);
    } else compute(toks.takeRight(toks.size - 1), st :+ toks.head.toInt);
  }

// test cases
// compute(syard(split("3 + 4 * ( 2 - 1 )")))   // 7
// compute(syard(split("10 + 12 * 33")))       // 406
// compute(syard(split("( 5 + 7 ) * 2")))      // 24
// compute(syard(split("5 + 7 / 2")))          // 8
// compute(syard(split("5 * 7 / 2")))          // 17
// compute(syard(split("9 + 24 / ( 7 - 3 )"))) // 15
// compute(syard(split("4 ^ 3 ^ 2")))      // 262144
// compute(syard(split("4 ^ ( 3 ^ 2 )")))  // 262144
// compute(syard(split("( 4 ^ 3 ) ^ 2")))  // 4096
// compute(syard(split("( 3 + 1 ) ^ 2 ^ 3")))   // 65536

  // C3b.compute(C3b.syard(C3b.split("4 ^ 3 ^ 2")))
  // C3b.compute(C3b.syard(C3b.split("( 3 + 1 ) ^ 2 ^ 3")))

}

// This template code is subject to copyright
// by King's College London, 2022. Do not
// make the template code public in any shape
// or form, and do not exchange it with other
// students under any circumstance.

object C2 { 

// ADD YOUR CODE BELOW
//======================

//(1)
def clean(s: String) : List[String] = {
  val res = """[0-9a-zA-Z-]+""".r.findAllIn(s);
  res.toList
}
  


//(2)
def occurrences(xs: List[String]): Map[String, Int] = {
  val cnt = xs.map( c => (c , xs.count( _ == c ) ) );
  cnt.toMap
}


//(3)

def prod(lst1: List[String], lst2: List[String]) : Int = {
  val m1 = occurrences(lst1) : Map[String,Int];
  val m2 = occurrences(lst2) : Map[String,Int];
  val st1 = lst1.distinct;
  //(st1).map( s=> println(s"${s} : ${m1.get(s).getOrElse(0)} , ${m2.get(s).getOrElse(0)} , ${m1.get(s).getOrElse(0) * m2.get(s).getOrElse(0)}"));
  val lst = ( st1 ).map( s => m1.get(s).getOrElse(0) * m2.get(s).getOrElse(0) ) : List[Int];
  lst.sum
}


//(4)
def overlap(lst1: List[String], lst2: List[String]) : Double = {
  val d = prod(lst1 , lst2);
  val mx = (prod(lst1,lst1)).max(prod(lst2,lst2));
  
  d.toDouble / mx.toDouble
}

def similarity(s1: String, s2: String) : Double = {
  val l1 = clean(s1);
  val l2 = clean(s2);
  overlap(l1,l2)
}

}

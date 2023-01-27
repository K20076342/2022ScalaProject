object M4a {

// If you need any auxiliary functions, feel free to 
// implement them, but do not make any changes to the
// templates below. Also have a look whether the functions
// at the end of the file are of any help.

type Pos = (Int, Int)    // a position on a chessboard 
type Path = List[Pos]    // a path...a list of positions

// ADD YOUR CODE BELOW
//======================



//(1) 
def is_legal(dim: Int, path: Path, x: Pos) : Boolean = {
  (0<= x._1) && (x._1 < dim) && (0<= x._2) && (x._2 < dim) && !path.contains(x)
}

val dir = List( 
  (  1,   2),
  (  2,   1),
  (  2, - 1),
  (  1, - 2),
  (- 1, - 2),
  (- 2, - 1),
  (- 2,   1),
  (- 1,   2) 
)
//(2) 
def legal_moves(dim: Int, path: Path, x: Pos) : List[Pos] = {
  val raw = dir.map( v => ( x._1+v._1 , x._2+v._2 ) ).toList
  val ok = raw.filter( i => is_legal( dim , path , i) )
  ok
}


//some testcases
/*
assert(legal_moves(8, Nil, (2,2)) == 
 List((3,4), (4,3), (4,1), (3,0), (1,0), (0,1), (0,3), (1,4)))
assert(legal_moves(8, Nil, (7,7)) == List((6,5), (5,6)))
assert(legal_moves(8, List((4,1), (1,0)), (2,2)) == List((3,4), (4,3), (3,0), (0,1), (0,3), (1,4)))
assert(legal_moves(8, List((6,6)), (7,7)) == List((6,5), (5,6)))
*/


// (3) 
def count_tours(dim: Int, path: Path) : Int = {
  if(path.size == dim*dim){
    1
  }
  else{
    val nxt_tour = legal_moves(dim, path, path(0) );
    val all_poss = nxt_tour.map( x => count_tours( dim , List(x)++path) ).toList;
    all_poss.sum
  }
}

def enum_tours(dim: Int, path: Path) : List[Path] = {
  if(path.size == dim*dim){
    List(path)
  }
  else{
    val nxt_tour = legal_moves(dim, path, path(0) );
    val all_poss = nxt_tour.map( x => enum_tours( dim , List(x)++path) ).flatten;
    all_poss
  }
}

// (4) 
def first(xs: List[Pos], f: Pos => Option[Path]) : Option[Path] = {
  if( xs== Nil ){
    None
  }
  else{
    val tmp = f(xs(0));

    if( tmp == None ){
      first( xs.takeRight( xs.size-1 ) , f )
    }
    else{
      tmp
    }
    
  }
}


// testcases
//
//def foo(x: (Int, Int)) = if (x._1 > 3) Some(List(x)) else None
//
//first(List((1, 0),(2, 0),(3, 0),(4, 0)), foo)   // Some(List((4,0)))
//first(List((1, 0),(2, 0),(3, 0)), foo)          // None


//(5) 
def first_tour(dim: Int, path: Path) : Option[Path] = {
  if( path.size == dim*dim ){
    Some(path)
  }
  else{
    first(legal_moves(dim, path, path(0) ), nxt => first_tour(dim, List( nxt ) ++path))
  }
}
 


/* Helper functions
// for measuring time
def time_needed[T](code: => T) : T = {
  val start = System.nanoTime()
  val result = code
  val end = System.nanoTime()
  println(f"Time needed: ${(end - start) / 1.0e9}%3.3f secs.")
  result
}
// can be called for example with
//
//     time_needed(count_tours(dim, List((0, 0))))
//
// in order to print out the time that is needed for 
// running count_tours
// for printing a board
def print_board(dim: Int, path: Path): Unit = {
  println()
  for (i <- 0 until dim) {
    for (j <- 0 until dim) {
      print(f"${path.reverse.indexOf((j, dim - i - 1))}%3.0f ")
    }
    println()
  } 
}
*/

}

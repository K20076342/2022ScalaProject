object M4b {

// !!! Copy any function you need from file knight1.scala !!!
//
// If you need any auxiliary functions, feel free to 
// implement them, but do not make any changes to the
// templates below.

type Pos = (Int, Int)    // a position on a chessboard 
type Path = List[Pos]    // a path...a list of positions

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
);

  def legal_moves(dim: Int, path: Path, x: Pos) : List[Pos] = {
  val raw = dir.map( v => ( x._1+v._1 , x._2+v._2 ) ).toList;
  val ok = raw.filter( i => is_legal( dim , path , i) );
  ok
}


// ADD YOUR CODE BELOW
//======================

//(6) 
def ordered_moves(dim: Int, path: Path, x: Pos) : List[Pos] = {
  val unsorted = legal_moves(dim,path,x);
  val res = unsorted.sortBy( p => legal_moves(dim,path,p).size );
  res
}



def tour_helper(dim: Int, OrigPaths : List[Path], isEnd: Boolean) : Option[Path] = {
  if( OrigPaths.size == 0 ){
    None 
  }
  else{
    val path = OrigPaths(0);
    val hd = path(0);
    val lst= path(path.size-1);
    
    val nxt_move = dir.map( v => ( hd._1+v._1 , hd._2+v._2 ) ).toList;
    
    if( path.size == dim*dim && (!isEnd || nxt_move.contains(lst) ) ){
      Some(path)
    }
    else{
      val ord_mov = ordered_moves(dim, path, hd );
      val res = ord_mov.map( x => List(x)++path ):::OrigPaths.takeRight( OrigPaths.size-1 ) ;
      tour_helper( dim , res , isEnd )
    }
  }
  
}

//(7) Complete the function that searches for a single *closed*
//    tour using the ordered_moves function from (6). This
//    function will be tested on a 6 x 6 board.
def first_closed_tour_heuristics(dim: Int, path: Path) : Option[Path] = {
  tour_helper(dim, path::Nil, true)
}

//(8) Same as (7) but searches for *non-closed* tours. This 
//    version of the function will be called with dimensions of 
//    up to 30 * 30.

def first_tour_heuristics(dim: Int, path: Path) : Option[Path] = {
  tour_helper(dim, path::Nil, false)
}

}




// This template code is subject to copyright 
// by King's College London, 2022. Do not 
// make the template code public in any shape 
// or form, and do not exchange it with other 
// students under any circumstance.

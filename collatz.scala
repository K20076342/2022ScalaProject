object C1 {
  
  //(1)
  def gen(num : Long) : Long = {
    if( num%2 == 1 ) 3*num+1;
    else num/2;
  }
  def collatz(num:Long) : Long = {
    if( num == 1 )  0;
    else{
      val res = gen(num);
      1+collatz(res);
    }
  }
//(2)

  def collatz_max(bnd:Int) : (Long,Long) = {
    val steps = (1 to bnd.toInt).map(collatz(_))
    val maximumSteps = steps.max
    (maximumSteps, steps.indexOf(maximumSteps) + 1)
  }

  //(3)
  def is_pow_of_two(n : Long ) : Boolean = {
    (n & (n-1)) == 0 
  }

   def is_hard(n : Long ) : Boolean = {
    is_pow_of_two((3*n)+1)
  }

  def last_odd(n : Long ) : Long = {
  val num2 = (3*n) + 1
  if (n % 2 == 0) {
    last_odd(n/2)}
  else if (is_hard(n)) {
    n}
  else{
    last_odd(num2)
  }
  }
  
}

// This template code is subject to copyright 
// by King's College London, 2022. Do not 
// make the template code public in any shape 
// or form, and do not exchange it with other 
// students under any circumstance.

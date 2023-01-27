object M1 {

//two test portfolios

  val blchip_portfolio =List("GOOG", "AAPL", "MSFT", "IBM", "FB", "AMZN", "BIDU");
  val rstate_portfolio = List(
    "PLD",
    "PSA",
    "AMT",
    "AIV",
    "AVB",
    "BXP",
    "CCI",
    "DLR",
    "EQIX",
    "EQR",
    "ESS",
    "EXR",
    "FRT",
    "HCP"
  );

  import io.Source
  import scala.util._

// ADD YOUR CODE BELOW
//======================

// (1)

  def get_january_data(symbol: String, year: Int): List[String] = {
    val reg = "[0-9]{4}-01-[0-9]{2},[0-9]{2,3}.[0-9]{6}";
    val raw = Source.fromFile(symbol+".csv").getLines();
    val yar = raw.filter(_.contains(year.toString));
    val jan = yar.filter(_ matches reg).toList;
    jan
  }

// (2)
  def get_first_price(symbol: String, year: Int): Option[Double] = {
    val raw = Source.fromFile(symbol+".csv").getLines();
    val yar = raw.filter(_.slice(0,4) == year.toString).toList;
    if( yar.size == 0 ) None
    else{
      Some( yar.apply(0).split(',').apply(1).toDouble )
    }

    // val reg = "2012-01-[0-9]{2},[0-9]{2,3}.[0-9]{6}";
    // val raw = Source.fromFile(symbol+".csv").getLines().filter(_.contains(year.toString)).toList;
  }

// (3)

  def get_prices( portfolio: List[String], years: Range): List[List[Option[Double]]] = {
    val reg  ="[0-9]{4}-[0-9]{2}-[0-9]{2},[0-9]{2,3}.[0-9]{6}";
    // val res = portfolio
    //   .map(nm =>
    //     years.map( y => get_first_price(nm,y) ).toList
    //   )
    //   .toList;

    val res = years.map( y =>  portfolio.map(
        nm => get_first_price(nm,y)
      ).toList
    ).toList
    
    res
  }

// (4)

  def realVal(x: Option[Double]) : Double = x match {
      case Some(s) => s
      case None => 0
   }

  def hasVal( x : Option[Double]) : Boolean = x match {
      case Some(s) => true
      case None => false
   }
  
  def get_delta(price_old: Option[Double],price_new: Option[Double]): Option[Double] = {
    if( hasVal(price_old) && hasVal(price_new) && realVal(price_old) != 0.toDouble ){
      val pn = realVal( price_new );
      val po = realVal( price_old );
      Some( (pn - po) / po )
    }
    else None
  }

// (5)
  def get_deltas( data: List[List[Option[Double]]] ): List[List[Option[Double]]] = {
    val n = data.size;
    val m = data(0).size;
    val res = (1 to (n - 1))
      .map(i =>
        (0 to (m-1))
          .map(j =>
            get_delta(data.apply(i - 1).apply(j) ,data.apply(i).apply(j) )
          )
          .toList
      )
      .toList
    res

  }

// (6)
  def yearly_yield( data: List[List[Option[Double]]], balance: Long, index: Int ): Long = {
   
    val n_row = data(index).flatten.size
    val un = balance.toDouble / n_row
    val ans = balance + data(index).flatten.map( x => x*un ).sum.toLong
    ans
  }
  

// (7)
  def compound_yield( data: List[List[Option[Double]]], balance: Long, index: Int ): Long = {
    if (index < data.size) compound_yield(data, yearly_yield(data, balance, index), index +1)
    else
    {
      balance
    }
  }

  def investment( portfolios: List[String], years: Range, start_balance: Long ): Long = {
   compound_yield(get_deltas(get_prices(portfolios,years)),start_balance, 0)
  }

}

//Test cases for the two portfolios given above


//println("Real data: " + investment(rstate_portfolio, 1978 to 2019, 100))
//println("Blue data: " + investment(blchip_portfolio, 1978 to 2019, 100))

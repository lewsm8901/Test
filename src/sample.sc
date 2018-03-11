import Test.CND

val T = 6.5 / 24 / 252
val r = 0.005

def d1(S: Double, K: Double, sigma: Double) = {
  val d1 = (math.log(S / K) + (0.005 + math.pow(sigma, 2) / 2) * T) / (sigma * math.sqrt(T))
  d1
}

def d2(S: Double, K: Double, sigma: Double) =  {
  val d2 = (math.log(S / K) + (0.005 - math.pow(sigma, 2) / 2) * T) / (sigma * math.sqrt(T))
  d2
}

def BS(S: Double, K: Double, sigma: Double, opt: String): Double = {
  if(opt == "C") {
    S * CND(d1(S, K, sigma)) - K * math.exp(-r * T) * CND(d2(S, K, sigma))
  } else {
    K * math.exp(-r * T) * CND(-d2(S, K, sigma)) - S * CND(-d1(S, K, sigma))
  }
}

def volfinder(S: Double, K: Double, price: Double, opt: String): Double = {

  val initialGuess = .5
  val tolerance = 0.0001

  val fx = (x: Double) => BS(S, K, x, opt) - price
  val fxPrime = (x: Double) => (BS(S, K, x + tolerance, opt) - BS(S, K, x - tolerance, opt)) / (2 * tolerance)
  //math.exp(-math.pow(d1(S, K, x), 2) * 0.5) / math.sqrt(2 * math.Pi) * math.sqrt(T) * S / 100

  val root = NRMethod(fx, fxPrime, initialGuess, tolerance)

  root
}



//def BS(S: Double, K: Double, sigma: Double, opt: String): Double = {
//  val d1: Double = (math.log(S / K) + (r + 0.5 * math.pow(sigma, 2)) * T) / (sigma * math.sqrt(T))
//  val d2: Double = d1 - sigma * math.sqrt(T)
//
//  if(opt == "C") {
//    S * CND(d1) - K * math.exp(-r * T) * CND(d2)
//  } else {
//    K * math.exp(-r * T) * CND(-d2) - S * CND(-d1)
//  }
//}
//
//def volfinder(S: Double, K: Double, price: Double, opt: String) = {
//
//  val initialGuess = .5
//  val tolerance = 0.0001
//
//  val fx = (x: Double) => BS(S, K, x, opt) - price
//  val fxPrime = (x: Double) => (BS(S, K, x + tolerance, opt) - BS(S, K, x - tolerance, opt)) / (2 * tolerance)
//
//  val root = NRMethod(fx, fxPrime, initialGuess, tolerance)
//
//  (K, root)

//  if(opt == "C") {
//    val fx = (x: Double) => BS(S, K, x, opt) - (price - K * math.exp(-r * T) + S)
//    val fxPrime = (x: Double) => (BS(S, K, x + tolerance, opt) - BS(S, K, x - tolerance, opt)) / (2 * tolerance)
//
//    val root = NRMethod(fx, fxPrime, initialGuess, tolerance)
//
//    (K, root)
//
//  } else {
//    val fx = (x: Double) => BS(S, K, x, opt) - (price + K * math.exp(-r * T) - S)
//    val fxPrime = (x: Double) => (BS(S, K, x + tolerance, opt) - BS(S, K, x - tolerance, opt)) / (2 * tolerance)
//
//    val root = NRMethod(fx, fxPrime, initialGuess, tolerance)
//
//    (K, root)
//  }
//}

def NRMethod(fx: Double => Double, fxPrime: Double => Double, x: Double, tolerance: Double): Double = {
  var x1 = x
  var xNext = NREq(fx, fxPrime, x1)
  while (math.abs(xNext - x1) > tolerance) {
    x1 = xNext
    xNext = NREq(fx, fxPrime, x1)
  }
  xNext
}

def NREq(fx: Double => Double, fxPrime: Double => Double, x: Double): Double = {
  x - fx(x) / fxPrime(x)
}

var Sbid = 313

val strike = List(305, 307.5, 310, 312.5, 315, 317.5, 320, 322.5, 325, 327.5, 330)
val cbid = List(7.8, 5.35, 3.11, 1.37, .42, .06, .01, .00, .00, .00, .00)
val cask = List(7.97, 5.46, 3.13, 1.38, .43, .07, .02, .01, .01, .01, .01)
val pbid = List(.02, .04, .25, 1.00, 2.54, 4.66, 7.07, 9.56, 12.05, 14.4, 17.05)
val pask = List(.03, .05, .26, 1.01, 2.56, 4.73, 7.2, 9.68, 12.9, 14.8, 20.95)

val callbid = strike.zip(cbid)
val putbid = strike.zip(pbid)

val callask = strike.zip(cask)
val putask = strike.zip(pask)

var callbidvollist = List[Double]()
var putbidvollist = List[Double]()

for (i <- callbid) {
  callbidvollist = callbidvollist :+ volfinder(Sbid, i._1, i._2, "C")
}
for (i <- putbid) {
  putbidvollist = putbidvollist :+ volfinder(Sbid, i._1, i._2, "P")
}

//println(callbidvollist)
//println(putbidvollist)

var Sask = 313.05

var callaskvollist = List[Double]()
var putaskvollist = List[Double]()

for (i <- callask) {
  callaskvollist = callaskvollist :+ (volfinder(Sask, i._1, i._2, "C"))
}

//callask.map(_._1)

val calist = callask.map{case (x,y) => {volfinder(Sask, x, y, "C")}}

println(calist)

println(callaskvollist)

//for (i <- putask) {
//  putaskvollist = putaskvollist :+ volfinder(Sask, i._1, i._2, "P")
//}


//println(putaskvollist)
//
//println(callbidvollist)
//println(putbidvollist)

//val CVL = (strike zip callbidvollist) zip callaskvollist map {
//  case((x, y), z) => (x, y, z)
//}

//val PVL = (strike zip putbidvollist) zip putaskvollist map {
//  case((x, y), z) => (x, y, z)
//}

//def vol(K: Double, CP: String, ba: String) = {
//  if(CP== "C") {
//    if(ba == "bid") {
//      CVL.find(_._1 == K).get._2
//    } else if(ba == "ask") {
//      CVL.find(_._1 == K).get._3
//    } else {
//      println("N/A")
//    }
//  } else if(CP == "P") {
//    if(ba == "bid") {
//      PVL.find(_._1 == K).get._2
//    } else if(ba == "ask") {
//      PVL.find(_._1 == K).get._3
//    } else {
//      println("N/A")
//    }
//  } else {
//    println("N/A")
//  }
//}

//def cvol(K: Double) ={
//  (CVL.find(_._1 == K).get._2, CVL.find(_._1 == K).get._3)
//}

//println(s"option price = ${BS(313, 305, .45, "C")}")



//for(i <- strike)
//  println("Strike Price: " + i + "\nCall Bid Volatility: " + cvol(i)._1 + ", Call Ask Volatility: " + cvol(i)._2 + BigDecimal(BS(313, 312.5, cvol(317.5)._1, "C")).setScale(2, BigDecimal.RoundingMode.HALF_UP))

//for (i <- strike)
//  println("Strike Price : " + i + s"\nCall Bid Volatility : " + BigDecimal(cvol(i)._1).setScale(2, BigDecimal.RoundingMode.HALF_UP) + "Call Ask Volatility : " + cvol(i)._2 +
//    s"\nTheoritical Prices : Call Bid = " + BS(Sbid, i, cvol(i)._1, "C") + s"Call Ask = " + BS(Sask, i, cvol(i)._2, "C"))

//for (i <- strike)
//  println("Strike Prive : " + i + s"\nCall Bid Volatility : " + cvol(i)._1 + "Call Ask Volatility : " + cvol(i)._2 +
//    s"\nTheoritical Prices : Call Bid = ${BigDecimal(BS(Sbid, i, cvol(i)._1, "C")).setScale(2, BigDecimal.RoundingMode.HALF_UP)}, " +
//    s"Call Ask = ${BigDecimal(BS(Sask, i, cvol(i)._2, "C")).setScale(2, BigDecimal.RoundingMode.HALF_UP)}" + "\n")

//for (i <- CVL)
//  println("Strike Price : " + i._1 + s"\nCall Bid Volatility : " + i._2 + "Call Ask Volatility : " + i._3 +
//    s"\nTheoritical Prices : Call Bid = ${BigDecimal(BS(Sbid, i._1, i._2, "C")).setScale(2, BigDecimal.RoundingMode.HALF_UP)}, " +
//    s"Call Ask = ${BigDecimal(BS(Sask, i._1, i._3, "C")).setScale(2, BigDecimal.RoundingMode.HALF_UP)}" + "\n")
//for (i <- PVL)
//  println("Strike Price : " + i._1 + s"\nPut Bid Volatility : " + i._2 + "Put Ask Volatility : " + i._3 +
//    s"\nPut Bid = ${BigDecimal(BS(Sbid, i._1, i._2, "P")).setScale(2, BigDecimal.RoundingMode.HALF_UP)}, " +
//    s"Put Ask = ${BigDecimal(BS(Sask, i._1, i._3, "P")).setScale(2, BigDecimal.RoundingMode.HALF_UP)}" + "\n")

//var vola = volfinder(S, 305, 7.8, "C")
//volfinder(S, 307.5, 5.35, "C")
//volfinder(S, 310, 3.11, "C")
//volfinder(S, 312.5, 1.37, "C")
//volfinder(S, 315, .42, "C")
//volfinder(S, 317.5, .06, "C")
//volfinder(S, 320, .01, "C")
//volfinder(S, 322.5, .00, "C")
//volfinder(S, 325, .00, "C")
//volfinder(S, 327.5, .00, "C")
//volfinder(S, 330, .00, "C")

//volfinder(313, 305, 7.8, "C")
//volfinder(313, 307.5, 5.35, "C")
//volfinder(313, 310, 1.37, "C")
//volfinder(313, 312.5, 1.37, "C")
//volfinder(313, 315, 0.42, "C")
//volfinder(313, 317.5, 0.06, "C")
//volfinder(313, 320, 0.01, "C")
//volfinder(313, 322.5, 0.00, "C")
//volfinder(313, 325, 0.00, "C")
//volfinder(313, 327.5, 0.00, "C")
//volfinder(313, 320, 0.00, "C")
//
//
//volfinder(313, 305, 0.02, "P")
//volfinder(313, 307.5, 0.04, "P")
//volfinder(313, 310, 0.25, "P")
//volfinder(313, 312.5, 1, "P")
//volfinder(313, 315, 2.54, "P")
//volfinder(313, 317.5, 4.66, "P")
//volfinder(313, 320, 7.07, "P")
//volfinder(313, 322.5, 9.56, "P")
//volfinder(313, 325, 12.05, "P")
//volfinder(313, 327.5, 14.4, "P")
//volfinder(313, 320, 17.05, "P")

// construct vol curve using put-call parity

//bid vols
//List(volfinder(313, 305, .02, "C"), volfinder(313, 307.5, .04, "C"), volfinder(313, 310, .25, "C"), volfinder(313, 312.5, 1, "C"),
//  volfinder(313, 315, 2.54, "C"), volfinder(313, 317.5, 4.66, "C"), volfinder(313, 320, 7.07, "C"), volfinder(313, 322.5, 9.56, "C"),
//  volfinder(313, 325, 12.05, "C"), volfinder(313, 327.5, 14.4, "C"), volfinder(313, 330, 17.05, "C"))

//ask vols


//BS(313.025, 305, .3650, "P")
//
//List(volfinder(313.025, 330, 16.98, "P"), volfinder(313.025, 327.5, 14.48, "P"))
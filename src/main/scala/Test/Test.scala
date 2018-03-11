import org.apache.commons.math3.distribution.NormalDistribution

object Test extends App {

  val strike = List(305, 307.5, 310, 312.5, 315, 317.5, 320, 322.5, 325, 327.5, 330)
  val cbid = List(7.8, 5.35, 3.11, 1.37, .42, .06, .01, .00, .00, .00, .00)
  val cask = List(7.97, 5.46, 3.13, 1.38, .43, .07, .02, .01, .01, .01, .01)
  val pbid = List(.02, .04, .25, 1.00, 2.54, 4.66, 7.07, 9.56, 12.05, 14.4, 17.05)
  val pask = List(.03, .05, .26, 1.01, 2.56, 4.73, 7.2, 9.68, 12.9, 14.8, 20.95)
  val T = 6.5 / 24 / 252
  val r = 0.005

  def CND(x: Double) = {
    val ND = new NormalDistribution
    ND.cumulativeProbability(x)
  }

  def d1(S: Double, K: Double, sigma: Double) = {
    val d1 = (math.log(S / K) + (0.005 + math.pow(sigma, 2) / 2) * T) / (sigma * math.sqrt(T))
    d1
  }

  def d2(S: Double, K: Double, sigma: Double) = {
    val d2 = (math.log(S / K) + (0.005 - math.pow(sigma, 2) / 2) * T) / (sigma * math.sqrt(T))
    d2
  }

  def BS(S: Double, K: Double, sigma: Double, opt: String): Double = {
    if (opt == "C") {
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

    val root = NRMethod(fx, fxPrime, initialGuess, tolerance)

    root
  }

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
  val callbid = strike.zip(cbid)
  val putbid = strike.zip(pbid)

  var callbidvollist = callbid.map{case (x, y) => {volfinder(Sbid, x, y, "C")}}
  var putbidvollist = putbid.map{case (x, y) => {volfinder(Sbid, x, y, "P")}}

//  for (i <- callbid) {
//    callbidvollist = callbidvollist :+ (volfinder(Sbid, i._1, i._2, "C"))
//  }
//  for (i <- putbid) {
//    putbidvollist = putbidvollist :+ (volfinder(Sbid, i._1, i._2, "P"))
//  }

  var Sask = 313.05
  val callask = strike.zip(cask)
  val putask = strike.zip(pask)

  var callaskvollist = callask.map{case (x, y) => {volfinder(Sask, x, y, "C")}}
  var putaskvollist = putask.map{case (x, y) => {volfinder(Sask, x, y, "P")}}

//  for (i <- callask) {
//    callaskvollist = callaskvollist :+ (volfinder(Sask, i._1, i._2, "C"))
//  }
//  for (i <- putask) {
//    putaskvollist = putaskvollist :+ (volfinder(Sask, i._1, i._2, "P"))
//  }

  val CVL = (strike zip callbidvollist) zip callaskvollist map {
    case ((x, y), z) => (x, y, z)
  }

  val PVL = (strike zip putbidvollist) zip putaskvollist map {
    case ((x, y), z) => (x, y, z)
  }

  def cvol(K: Double) = {
    (CVL.find(_._1 == K).get._2, CVL.find(_._1 == K).get._3)
  }

  def pvol(K: Double) = {
    (PVL.find(_._1 == K).get._2, PVL.find(_._1 == K).get._3)
  }

  for (i <- strike)
    println("Strike Price : " + i + s"\nCall Bid Volatility : " + cvol(i)._1 + " Call Ask Volatility : " + cvol(i)._2 +
      s"\nCall Theoretical Prices : Call Bid = " + BS(Sbid, i, cvol(i)._1, "C") + s", Call Ask = " + BS(Sask, i, cvol(i)._2, "C") +
      s"\nTheoretical Prices : Put Bid = " + BS(Sbid, i, pvol(i)._1, "P") + s", Put Ask = " + BS(Sask, i, pvol(i)._2, "P") + "\n")

}
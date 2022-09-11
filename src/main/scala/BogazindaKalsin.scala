
object BogazindaKalsin extends App {
  val probabilites = (BigDecimal(0.05) to BigDecimal(0.5) by BigDecimal(0.05)).map(_.toDouble).toList   // 0.05, 0.10, 0.15, 0.20 ...
  val (rand, gameCount, totalWealth, eachProbTestCount) = (scala.util.Random, 1000, 100000L, 1)

  def isAlwaysWinner(gameCount: Int, totalWealth: Long, probability: Double):Boolean = {
    @scala.annotation.tailrec def acc(nextRand:Double, gameCount:Int, currentWealth:Double):Boolean = {
      nextRand match {
        case _ if probability < nextRand => // losing
          val nextWealth = currentWealth - math.pow(2, gameCount)
          nextWealth match {
            case value if value >= 0=> acc(rand.nextFloat, gameCount + 1, nextWealth) // can play still
            case _ => println(s""" isWinner = ${false} gameCount: $gameCount currentWealth: $currentWealth latestWealth: $currentWealth probability: $probability """.stripMargin)
              false
          }
        case _ => // winning
          val nextWealth = currentWealth + math.pow(2, gameCount)
          val isWinner = totalWealth < nextWealth
          println(s""" isWinner = $isWinner gameCount: $gameCount currentWealth: $currentWealth latestWealth: $nextWealth probability: $probability """.stripMargin)
          isWinner
      }
    }
    acc(rand.nextFloat, gameCount + 1, totalWealth - math.pow(2, gameCount))
  }
  probabilites.foreach(l => (1 to eachProbTestCount).foreach { _ =>
    isAlwaysWinner(0, totalWealth, l)
  })
}

package chapter6

import org.scalatest.{Matchers, WordSpec}

class Ex11bSpec extends WordSpec with Matchers {

  sealed trait Input

  case object Coin extends Input

  case object Turn extends Input

  case class Machine(locked: Boolean, candies: Int, coins: Int)

  private implicit class MachineOps(machine: Machine) {

    private lazy val unlocked = !machine.locked

    def takeCoinIfLocked =
      if (machine.locked) machine.copy(locked = false, coins = machine.coins + 1)
      else machine

    def getCandyIfUnlocked =
      if (unlocked) machine.copy(locked = true, candies = machine.candies - 1)
      else machine

    lazy val toCoinCandyMachine = (machine.coins -> machine.candies) -> machine
  }

  type MachineState = State.State[Machine, (Int, Int)]

  def simulateMachine(inputs: Input*): State[Machine, (Int, Int)] =
    inputs.foldLeft(State[Machine, (Int, Int)](m => (m.coins -> m.candies) -> m))(takeInput)

  private val takeInput: (State[Machine, (Int, Int)], Input) => State[Machine, (Int, Int)] = {
    case (s, Coin) => State(s.flatMap(_ => machine => machine.takeCoinIfLocked.toCoinCandyMachine))
    case (s, Turn) => State(s.flatMap(_ => machine => machine.getCandyIfUnlocked.toCoinCandyMachine))
    case (s, _) => State(s.flatMap(_ => machine => machine.toCoinCandyMachine))
  }
  
  "simulateMachine" should {

    "return 14 coins and 1 candy " +
      "when 4 candies successfully bought from machine having 10 coins and 5 candies initially" in {

      val machine = Machine(locked = true, candies = 5, coins = 10)

      val inputs = List.fill(4)(List(Coin, Turn)).flatten

      val ((coinsLeft, candiesLeft), _) = simulateMachine(inputs: _*).run(machine)

      coinsLeft shouldBe 14
      candiesLeft shouldBe 1
    }

    "do nothing " +
      "when turned on locked machine" in {

      val machine = Machine(locked = true, candies = 5, coins = 10)

      val ((coinsLeft, candiesLeft), _) = simulateMachine(Turn).run(machine)

      coinsLeft shouldBe 10
      candiesLeft shouldBe 5
    }

    "do nothing " +
      "when given coin on unlocked machine" in {

      val machine = Machine(locked = false, candies = 5, coins = 10)

      val ((coinsLeft, candiesLeft), _) = simulateMachine(Coin).run(machine)

      coinsLeft shouldBe 10
      candiesLeft shouldBe 5
    }
  }
}
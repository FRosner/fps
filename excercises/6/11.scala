import Machine._

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: MachineLocked, candies: NumCandies, coins: NumCoins)

object Machine {
  type NumCandies = Int
  type NumCoins = Int
  type MachineLocked = Boolean

  type MachineState = State[Machine, (NumCoins, NumCandies)]

  // val withCandies(numCandies: NumCandies): State[Machine, (NumCoins, NumCandies)] =
  //   State.unit[Machine, (NumCoins, NumCandies)]((0, numCandies))
}

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    flatMap(a => State.unit(f(a)))

  def map2[B, C](rb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => rb.map(b => f(a, b)))

  def flatMap[B](g: A => State[S, B]): State[S, B] = State(
    (s: S) => {
      val (a, s2) = run(s)
      g(a).run(s2)
    }
  )

  def modify(f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def get: State[S, S] = State(s => (s, s))

  def set(s: S): State[S, Unit] = State(_ => ((), s))
}

object State {
  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.foldRight(unit[S, List[A]](List.empty[A]))((f, acc) => f.map2(acc)(_ :: _))
}

object Main extends App {
  // Inserting a coin into a locked machine will cause it to unlock if there's any candy left
  // Turning the knob on an unlocked machine will cause it to dispense candy and become locked
  // Turning the knob on a locked machine or inserting a coin into an unlocked machine does nothing
  // A machine that's out of candy ignores all inputs
  def simulateInput(input: Input): MachineState = State {
    m => {
      val newM = {
        if (m.candies <= 0) {
          m
        } else if (input == Coin) {
          if (m.locked && m.candies > 0)
            m.copy(locked = false)
          else
            m
        } else { // input == Turn
          if (m.locked)
            m
          else
            m.copy(locked = true, candies = m.candies - 1)
        }
      }
      ((newM.coins, newM.candies), newM)
    }
  }

  // Example: If the input machine has 10 coins and 5 candies and a total of 4 candies are bought, it should return (14,1)
  def simulateMachine(inputs: List[Input]): MachineState =
    State.sequence(inputs.map(i => simulateInput(i))).map(_.last)

  val plusOne = State[Int, Int](s => (s, s + 1))
  val res = for {
    x <- plusOne
    y <- plusOne
  } yield (x, y)
  println(res.run(0))

  val transitions = for {
    x <- simulateInput(Coin)
    y <- simulateInput(Turn)
  } yield y
  println(transitions.run(Machine(true, 10, 0)))

  println(simulateMachine(List(Coin, Turn)).run(Machine(true, 10 ,0)))

}

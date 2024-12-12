object Main extends App {
	System.out.println("Hello World!");
}

sealed trait Firework

object Firework:

  /** @return
    *   A random initial state of a firework
    */
  def init(): Firework = Waiting.init()

  /** @return
    *   The next state of a firework
    * @param firework
    *   The current state of a firework
    *
    * If the firework is in the [[Done]] state, it stays in the same [[Done]] state. Otherwise, the next state is
    * computed by calling the operation `next` on the underlying state type (see e.g., [[Waiting.next]]).
    *
    * Hint: choose what to do by pattern matching on the given `firework`. You will have to use “typed patterns” to
    * match on case classes and “literal patterns” to match on case objects.
    */
  def next(firework: Firework): Firework =
    firework match
      case w: Waiting   => w.next
      case l: Launched  => l.next
      case e: Exploding => e.next
      case Done         => firework

end Firework

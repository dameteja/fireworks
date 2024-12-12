package fireworks

import scala.util.Random

// Define an Angle class with basic functionality and support for addition
case class Angle(radians: Double) {
  def cos: Double = math.cos(radians)
  def sin: Double = math.sin(radians)

  // Add operator to support addition of two angles
  def +(other: Angle): Angle = Angle(this.radians + other.radians)
}

object Angle {
  def apply(degrees: Double): Angle = new Angle(math.toRadians(degrees))
}

// Define a Color class as a placeholder
case class Color(red: Int, green: Int, blue: Int)

object Color {
  val red: Color = Color(255, 0, 0)
  val yellow: Color = Color(255, 255, 0)
  val white: Color = Color(255, 255, 255)
  val blue: Color = Color(0, 0, 255)
  val violet: Color = Color(238, 130, 238)
}

// Define a Point class to represent 2D points
case class Point(x: Double, y: Double)

/** A firework can be in the following states:
  *
  *   - [[Waiting]] to be launched,
  *   - [[Launched]] (it has not exploded yet, it’s going up in the sky),
  *   - [[Exploding]],
  *   - [[Done]] (all the particles have burnt).
  */
sealed trait Firework

object Firework:

  def init(): Firework = Waiting.init()

  def next(firework: Firework): Firework =
    firework match
      case w: Waiting   => w.next
      case l: Launched  => l.next
      case e: Exploding => e.next
      case Done         => firework

end Firework

case class Waiting(countDown: Int, startPosition: Point, numberOfParticles: Int, particlesColor: Color)
    extends Firework:
  def next: Firework =
    if countDown > 0 then copy(countDown = countDown - 1)
    else Launched.init(startPosition, numberOfParticles, particlesColor)

end Waiting

object Waiting:
  def init(): Waiting =
    val color = Settings.colors(Random.nextInt(Settings.colors.length))
    val numberOfParticles = 40
    val position = Point(
      (Random.nextInt(Settings.width / 2) - Settings.width / 4).toDouble,
      (-Settings.height / 2).toDouble
    )
    val countDown = Random.nextInt(60)
    Waiting(countDown, position, numberOfParticles, color)

end Waiting

case class Launched(countDown: Int, position: Point, direction: Angle, numberOfParticles: Int, particlesColor: Color)
    extends Firework:
  def next: Firework =
    if countDown > 0 then
      copy(countDown = countDown - 1, position = Motion.movePoint(position, direction, Settings.propulsionSpeed))
    else Exploding.init(numberOfParticles, direction, position, particlesColor)

end Launched

object Launched:
  def init(position: Point, numberOfParticles: Int, particlesColor: Color): Launched =
    val direction = Angle(math.Pi / 2 + (Random.nextDouble() - 0.5) / 5)
    Launched(countDown = 30, position, direction, numberOfParticles, particlesColor)

end Launched

case class Exploding(countDown: Int, particles: Particles) extends Firework:
  def next: Firework =
    if countDown > 0 then copy(countDown = countDown - 1, particles = particles.next)
    else Done

end Exploding

object Exploding:
  def init(numberOfParticles: Int, direction: Angle, position: Point, color: Color): Exploding =
    val particles = List.fill(numberOfParticles)(Particle.init(direction, position, color))
    Exploding(countDown = 30, Particles(particles))

end Exploding

case object Done extends Firework

case class Particle(horizontalSpeed: Double, verticalSpeed: Double, position: Point, color: Color):
  def next: Particle =
    val updatedHorizontalSpeed = Motion.drag(horizontalSpeed)
    val updatedVerticalSpeed = Motion.drag(verticalSpeed - Settings.gravity)
    val updatedPosition = Point(position.x + updatedHorizontalSpeed, position.y + updatedVerticalSpeed)
    copy(horizontalSpeed = updatedHorizontalSpeed, verticalSpeed = updatedVerticalSpeed, position = updatedPosition)

end Particle

case class Particles(value: List[Particle]):
  def next: Particles = Particles(value.map(particle => particle.next))

end Particles

object Particle:
  def init(initialDirection: Angle, position: Point, color: Color): Particle =
    val angle = initialDirection + Angle(Random.nextDouble() * (math.Pi / 4) - math.Pi / 8)
    val velocity = Random.nextDouble() * 10 + 20
    Particle(angle.cos * velocity, angle.sin * velocity, position, color)

end Particle

object Motion:
  def movePoint(point: Point, direction: Angle, speed: Double): Point =
    Point(point.x + direction.cos * speed, point.y + direction.sin * speed)

  def drag(speed: Double): Double =
    if speed > Settings.friction then math.max(speed - Settings.friction, 0)
    else if speed < -Settings.friction then math.min(speed + Settings.friction, 0)
    else 0

end Motion

object Settings:
  val width = 800
  val height = 600
  val colors: Array[Color] = Array(Color.red, Color.yellow, Color.white, Color.blue, Color.violet)

  val friction = 0.2
  val gravity = 1.5
  val propulsionSpeed = 8.0

end Settings

// Entry point for the program
object Main:
  def main(args: Array[String]): Unit =
    var firework: Firework = Firework.init()
    println("Starting Firework Simulation!")

    for (_ <- 1 to 100) do
      firework = Firework.next(firework)
      println(firework) // Print the current state of the firework

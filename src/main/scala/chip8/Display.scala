package chip8

import chip8.Display.Position

import scala.annotation.tailrec


case class Display(
  private val data: Vector[Vector[Boolean]],
  private val charPixel: Char = '@') {

  private lazy val width = data.length
  private lazy val height = data(0).length

  override lazy val toString: String =
    (for {
      h <- data(0).indices
      w <- data.indices
      c = if (data(w)(h)) charPixel else ' '
    } yield if (w == data.length - 1) c + "\n" else c).mkString

  def update(pos: Position, value: Boolean): Display =
    copy(data = data.updated(pos._1, data(pos._1).updated(pos._2, value)))

  def apply(pos: Position): Boolean = data(pos._1)(pos._2)

  def clear: Display = copy(data = Vector.fill(width, height)(false))

  def draw(x: Short, y: Short, sprite: Seq[Byte]): (Byte, Display) = {
    @tailrec def draw_(i: Int, j: Int, coll: Byte, data: Vector[Vector[Boolean]]): (Byte, Display) = {
      if (i >= sprite.length)
        (coll, copy(data = data))
      else {
        val xi = i + x
        val yj = j + y
        if (xi >= width || yj >= height)
          draw_(if (j == 7) i + 1 else i, (j + 1) % 8, coll, data)
        else {
          val mask = 1 << j
          val bit = (sprite(i) & mask) == mask
          val coll_ = if (data(xi)(yj)) 1: Byte else coll
          val data_ = data.updated(xi, data(xi).updated(yj, bit))
          draw_(if (j == 7) i + 1 else i, (j + 1) % 8, coll_, data_)
        }
      }
    }
    draw_(0, 0, 0, data)
  }

}


private object Display {

  type Position = (Int, Int)

  def empty = Display(Vector.fill(screenSize._1, screenSize._2)(false))

}
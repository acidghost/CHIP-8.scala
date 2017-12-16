package chip8

private case class Stack(private val data: Vector[Short]) {

  def push(address: Short): Stack = copy(data = data :+ address)

  def top: Option[Short] = data.lastOption

  def pop: Option[(Short, Stack)] = top map ((_, copy(data = data.init)))

}

private object Stack {

  def empty = Stack(Vector())

}

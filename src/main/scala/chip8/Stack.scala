package chip8

import chip8.unsigned.UShort

private case class Stack(private val data: Vector[UShort]) {

  def push(address: UShort): Stack = copy(data = data :+ address)

  def top: Option[UShort] = data.lastOption

  def pop: Option[(UShort, Stack)] = top map ((_, copy(data = data.init)))

}

private object Stack {

  def empty = Stack(Vector())

}

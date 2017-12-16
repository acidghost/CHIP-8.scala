package chip8

import chip8.Registers.Register


private case class Registers(private val data: Vector[Byte]) {

  def apply(register: Register): Byte = data(register.index)

  def update(register: Register, value: Byte): Registers =
    copy(data = data.updated(register.index, value))

}


private object Registers {

  def empty = Registers(Vector.fill(16)(0))

  def fromNum(n: Short): Option[Register] = n match {
    case 0  => Some(V0)
    case 1  => Some(V1)
    case 2  => Some(V2)
    case 3  => Some(V3)
    case 4  => Some(V4)
    case 5  => Some(V5)
    case 6  => Some(V6)
    case 7  => Some(V7)
    case 8  => Some(V8)
    case 9  => Some(V9)
    case 10 => Some(VA)
    case 11 => Some(VB)
    case 12 => Some(VC)
    case 13 => Some(VD)
    case 14 => Some(VE)
    case 15 => Some(VF)
    case _  => None
  }

  abstract sealed class Register(val index: Int)

  final case object V0 extends Register(0)
  final case object V1 extends Register(1)
  final case object V2 extends Register(2)
  final case object V3 extends Register(3)
  final case object V4 extends Register(4)
  final case object V5 extends Register(5)
  final case object V6 extends Register(6)
  final case object V7 extends Register(7)
  final case object V8 extends Register(8)
  final case object V9 extends Register(9)
  final case object VA extends Register(10)
  final case object VB extends Register(11)
  final case object VC extends Register(12)
  final case object VD extends Register(13)
  final case object VE extends Register(14)
  final case object VF extends Register(15)

}

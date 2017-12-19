package chip8

import chip8.unsigned.{UByte, UShort}
import chip8.unsigned.UByte.UByteIsNumeric


private case class Memory(private val data: Vector[UByte]) {

  def apply(idx: Int): UByte = data(idx)
  def apply(idx: UShort): UByte = data(idx.toInt)

  def update(idx: Int, value: UByte): Memory =
    copy(data = data.updated(idx, value))

  def append(value: UByte): Memory = copy(data = data :+ value)

  def slice(from: Int, to: Int) = data.slice(from, to)

}


private object Memory {

  def empty = Memory(Vector.fill(availableMemory)(UByte(0)))

  def withFontSet = fontSet.foldLeft(Memory.empty)(_ append _)

  def load(bytes: Seq[Byte], at: Short = startAddress) =
    bytes.zipWithIndex.foldLeft(Memory.withFontSet) {
      case (mem, (b, i)) => mem(at + i) = UByte(b)
    }

  val fontSet: Seq[UByte] = Seq(
    0xF0, 0x90, 0x90, 0x90, 0xF0,       // 0
    0x20, 0x60, 0x20, 0x20, 0x70,       // 1
    0xF0, 0x10, 0xF0, 0x80, 0xF0,       // 2
    0xF0, 0x10, 0xF0, 0x10, 0xF0,       // 3
    0x90, 0x90, 0xF0, 0x10, 0x10,       // 4
    0xF0, 0x80, 0xF0, 0x10, 0xF0,       // 5
    0xF0, 0x80, 0xF0, 0x90, 0xF0,       // 6
    0xF0, 0x10, 0x20, 0x40, 0x40,       // 7
    0xF0, 0x90, 0xF0, 0x90, 0xF0,       // 8
    0xF0, 0x90, 0xF0, 0x10, 0xF0,       // 9
    0xF0, 0x90, 0xF0, 0x90, 0x90,       // A
    0xE0, 0x90, 0xE0, 0x90, 0xE0,       // B
    0xF0, 0x80, 0x80, 0x80, 0xF0,       // C
    0xE0, 0x90, 0x90, 0x90, 0xE0,       // D
    0xF0, 0x80, 0xF0, 0x80, 0xF0,       // E
    0xF0, 0x80, 0xF0, 0x80, 0x80        // F
  ) map UByteIsNumeric.fromInt

}

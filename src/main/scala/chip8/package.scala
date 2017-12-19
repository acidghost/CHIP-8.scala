import chip8.unsigned.{UByte, UShort}

package object chip8 {

  val startAddress: Short = 0x200
  val availableMemory = 4096
  val screenSize = (64, 32)

  implicit class ShortOps(short: Short) {
    def toUByte: UByte = UByte(short)
    def toUShort: UShort = UShort(short)
  }

  implicit class IntOps(int: Int) {
    def toUByte: UByte = UByte(int)
    def toUShort: UShort = UShort(int)
  }


}

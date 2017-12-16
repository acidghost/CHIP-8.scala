package object chip8 {

  val startAddress: Short = 0x200
  val availableMemory = 4096
  val screenSize = (64, 32)

  implicit class ShortOps(short: Short) {
    def <+>(i: Int): Short = (short + i).toShort
    def <->(i: Int): Short = (short - i).toShort
    def <*>(i: Int): Short = (short * i).toShort
    def <&>(i: Int): Short = (short & i).toShort
    def <>>(i: Int): Short = (short >> i).toShort
  }

}

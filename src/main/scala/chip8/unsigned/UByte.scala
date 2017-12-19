package chip8.unsigned

import scala.math.pow


object UByte {

  @inline final def apply(n: Byte): UByte = new UByte(n)
  @inline final def apply(n: Int): UByte = new UByte(n.toByte)

  @inline final def MinValue: UByte = UByte(0)
  @inline final def MaxValue: UByte = UByte(-1)

  implicit object UByteIsNumeric extends UByteIsNumeric

  trait UByteIsNumeric extends Numeric[UByte] {
    override def plus(x: UByte, y: UByte): UByte = x + y
    override def minus(x: UByte, y: UByte): UByte = x - y
    override def times(x: UByte, y: UByte): UByte = x * y
    override def negate(x: UByte): UByte = ~x

    override def fromInt(x: Int): UByte = UByte((x & 0xff).toByte)

    override def toInt(x: UByte): Int = x.toInt
    override def toLong(x: UByte): Long = x.toLong
    override def toFloat(x: UByte): Float = x.toFloat
    override def toDouble(x: UByte): Double = x.toDouble

    override def compare(x: UByte, y: UByte): Int =
      if (x > y) 1
      else if (x < y) -1
      else 0
  }

}

class UByte(val signed: Byte) extends AnyVal
  with scala.math.ScalaNumericAnyConversions {

  override def toByte: Byte = signed
  override def toChar: Char = (signed & 0xff).toChar
  override def toShort: Short = (signed & 0xff).toShort
  override def toInt: Int = signed & 0xff
  override def toLong: Long = signed & 0xffL
  override def toFloat: Float = toInt.toFloat
  override def toDouble: Double = toInt.toDouble

  def toUShort: UShort = UShort(toShort)

  override def isWhole(): Boolean = true
  override def underlying(): Any = signed
  override def byteValue(): Byte = toByte
  override def shortValue(): Short = toShort
  override def intValue(): Int = toInt
  override def longValue(): Long = toLong
  override def floatValue(): Float = toFloat
  override def doubleValue(): Double = toDouble

  override def toString: String = toInt.toString
  def toHexString: String = (signed & 0xff).toHexString

  def == (that: UByte): Boolean = this.signed == that.signed
  def != (that: UByte): Boolean = this.signed != that.signed

  def ===(that: UByte): Boolean = this.signed == that.signed
  def =!=(that: UByte): Boolean = this.signed != that.signed

  def <= (that: UByte): Boolean = this.toInt <= that.toInt
  def < (that: UByte): Boolean = this.toInt < that.toInt
  def >= (that: UByte): Boolean = this.toInt >= that.toInt
  def > (that: UByte): Boolean = this.toInt > that.toInt

  def unary_- : UByte = UByte(-this.signed)

  def + (that: UByte): UByte = UByte(this.signed + that.signed)
  def - (that: UByte): UByte = UByte(this.signed - that.signed)
  def * (that: UByte): UByte = UByte(this.signed * that.signed)
  def / (that: UByte): UByte = UByte(this.toInt / that.toInt)
  def % (that: UByte): UByte = UByte(this.toInt % that.toInt)

  def unary_~ : UByte = UByte(~this.signed)

  def << (shift: Int): UByte = UByte((signed & 0xff) << (shift & 7))
  def >> (shift: Int): UByte = UByte((signed & 0xff) >>> (shift & 7))
  def >>> (shift: Int): UByte = UByte((signed & 0xff) >>> (shift & 7))
  def & (that: UByte): UByte = UByte((this.signed & 0xff) & (that.signed & 0xff))
  def | (that: UByte): UByte = UByte((this.signed & 0xff) | (that.signed & 0xff))
  def ^ (that: UByte): UByte = UByte((this.signed & 0xff) ^ (that.signed & 0xff))

  def ** (that: UByte): UByte = UByte(pow(this.toLong, that.toLong).toInt)

}

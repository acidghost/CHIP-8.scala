package chip8.unsigned

import scala.math.pow


object UShort {

  @inline final def apply(n: Char): UShort = new UShort(n)
  @inline final def apply(n: Short): UShort = new UShort(n.toChar)
  @inline final def apply(n: Int): UShort = new UShort(n.toChar)

  @inline final def MinValue: UShort = UShort(0)
  @inline final def MaxValue: UShort = UShort(Char.MaxValue)

  trait UShortIsNumeric extends Numeric[UShort] {
    override def plus(x: UShort, y: UShort): UShort = x + y
    override def minus(x: UShort, y: UShort): UShort = x + y
    override def times(x: UShort, y: UShort): UShort = x + y
    override def negate(x: UShort): UShort = ~x

    override def fromInt(x: Int): UShort = UShort(x)

    override def toInt(x: UShort): Int = x.toInt
    override def toLong(x: UShort): Long = x.toLong
    override def toFloat(x: UShort): Float = x.toFloat
    override def toDouble(x: UShort): Double = x.toDouble

    override def compare(x: UShort, y: UShort): Int =
      if (x > y) 1
      else if (x < y) -1
      else 0
  }

}

class UShort(val signed: Char) extends AnyVal
  with scala.math.ScalaNumericAnyConversions {

  override def toByte: Byte = signed.toByte
  override def toChar: Char = signed
  override def toShort: Short = signed.toShort
  override def toInt: Int = signed.toInt
  override def toLong: Long = signed.toLong
  override def toFloat: Float = signed.toFloat
  override def toDouble: Double = signed.toDouble

  def toUByte: UByte = UByte(toByte)

  override def isWhole(): Boolean = true
  override def underlying(): Any = signed
  override def byteValue(): Byte = toByte
  override def shortValue(): Short = toShort
  override def intValue(): Int = toInt
  override def longValue(): Long = toLong
  override def floatValue(): Float = toFloat
  override def doubleValue(): Double = toDouble

  override def toString: String = toInt.toString
  def toHexString: String = (signed & 0xffff).toHexString

  def == (that: UShort): Boolean = this.signed == that.signed
  def != (that: UShort): Boolean = this.signed != that.signed

  def ===(that: UShort): Boolean = this.signed == that.signed
  def =!=(that: UShort): Boolean = this.signed != that.signed

  def <= (that: UShort): Boolean = this.signed <= that.signed
  def < (that: UShort): Boolean = this.signed < that.signed
  def >= (that: UShort): Boolean = this.signed >= that.signed
  def > (that: UShort): Boolean = this.signed > that.signed

  def unary_- : UShort = UShort(-this.signed)

  def + (that: UShort): UShort = UShort(this.signed + that.signed)
  def - (that: UShort): UShort = UShort(this.signed - that.signed)
  def * (that: UShort): UShort = UShort(this.signed * that.signed)
  def / (that: UShort): UShort = UShort(this.signed / that.signed)
  def % (that: UShort): UShort = UShort(this.signed % that.signed)

  def unary_~ : UShort = UShort(~this.signed)

  def << (shift: Int): UShort = UShort((signed & 0xffff) << (shift & 15))
  def >> (shift: Int): UShort = UShort((signed & 0xffff) >>> (shift & 15))
  def >>> (shift: Int): UShort = UShort((signed & 0xffff) >>> (shift & 15))
  def & (that: UShort): UShort = UShort(this.signed & that.signed)
  def | (that: UShort): UShort = UShort(this.signed | that.signed)
  def ^ (that: UShort): UShort = UShort(this.signed ^ that.signed)

  def ** (that: UShort): UShort = UShort(pow(this.toLong, that.toLong).toChar)

}

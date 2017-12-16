package chip8

import java.util.concurrent.BlockingQueue

import cats.data.State
import chip8.Concurrency.{Consumer, QueueDrainer}
import chip8.Keypad.KeypadEvent

import scala.annotation.tailrec


/** Keypad layout
  * +---+---+---+---+       +---+---+---+---+
  * | 1 | 2 | 3 | C |       | 1 | 2 | 3 | 4 |
  * +---+---+---+---+       +---+---+---+---+
  * | 4 | 5 | 6 | D |       | Q | W | E | R |
  * +---+---+---+---+       +---+---+---+---+
  * | 7 | 8 | 9 | E |       | A | S | D | F |
  * +---+---+---+---+       +---+---+---+---+
  * | A | 0 | B | F |       | Z | X | C | V |
  * +---+---+---+---+       +---+---+---+---+
  */

object Keypad {

  sealed abstract class Key(val index: Int)

  final case object Key0 extends Key(0)
  final case object Key1 extends Key(1)
  final case object Key2 extends Key(2)
  final case object Key3 extends Key(3)
  final case object Key4 extends Key(4)
  final case object Key5 extends Key(5)
  final case object Key6 extends Key(6)
  final case object Key7 extends Key(7)
  final case object Key8 extends Key(8)
  final case object Key9 extends Key(9)
  final case object KeyA extends Key(10)
  final case object KeyB extends Key(11)
  final case object KeyC extends Key(12)
  final case object KeyD extends Key(13)
  final case object KeyE extends Key(14)
  final case object KeyF extends Key(15)


  type KeyMap[A] = PartialFunction[A, Keypad.Key]


  trait ToKey[A] {
    def toKey(in: A): Option[Key]
  }

  object ToKey {

    def apply[A](implicit tk: ToKey[A]): ToKey[A] = tk

    implicit class ToKeyOps[A: ToKey](a: A) {
      def toKey: Option[Key] = ToKey[A].toKey(a)
    }

    implicit class KeyMapOps[A](km: KeyMap[A]) {
      def toKey = new ToKey[A] {
        def toKey(in: A): Option[Key] = km lift in
      }
    }

    implicit def numToKey[N: Numeric]: ToKey[N] = ({
      case  0 => Key0
      case  1 => Key1
      case  2 => Key2
      case  3 => Key3
      case  4 => Key4
      case  5 => Key5
      case  6 => Key6
      case  7 => Key7
      case  8 => Key8
      case  9 => Key9
      case 10 => KeyA
      case 11 => KeyB
      case 12 => KeyC
      case 13 => KeyD
      case 14 => KeyE
      case 15 => KeyF
    }: KeyMap[N]).toKey

    implicit val charToKey: ToKey[Char] = ({
      case 'x' => Key0
      case '1' => Key1
      case '2' => Key2
      case '3' => Key3
      case 'q' => Key4
      case 'w' => Key5
      case 'e' => Key6
      case 'a' => Key7
      case 's' => Key8
      case 'd' => Key9
      case 'z' => KeyA
      case 'c' => KeyB
      case '4' => KeyC
      case 'r' => KeyD
      case 'f' => KeyE
      case 'v' => KeyF
    }: KeyMap[Char]).toKey

  }

  case class KeypadEvent(key: Key, pressed: Boolean)

  def empty(queue: BlockingQueue[KeypadEvent]) = Keypad(Vector.fill(16)(false), queue)

  private def apply(keys: Vector[Boolean], queue: BlockingQueue[KeypadEvent]): Keypad =
    new Keypad(keys, queue)

  val waitForKey: State[Keypad, Key] = State(_.waitForKey)

}


class Keypad private(
  private val keys: Vector[Boolean],
  override protected val queue: BlockingQueue[KeypadEvent]
) extends Consumer[KeypadEvent](queue)
  with QueueDrainer[KeypadEvent] {

  import Keypad.Key

  def pressed(key: Key): Boolean = keys(key.index)

  private def update(key: Key, value: Boolean): Keypad =
    Keypad(keys.updated(key.index, value), queue)

  private def update(keypadEvent: KeypadEvent): Keypad =
    update(keypadEvent.key, keypadEvent.pressed)

  def press(key: Key): Keypad = this(key) = true
  def release(key: Key): Keypad = this(key) = false

  override lazy val toString: String =
    keys map (b => if (b) '1' else '0') mkString ""

  def withDrainedQ: Keypad = drainQueue(keys)((keys, evt) => evt match {
      case KeypadEvent(key, pressed) => keys.updated(key.index, pressed)
    })(Keypad(_, queue))

  @tailrec final def waitForKey: (Keypad, Key) = {
    val drained = withDrainedQ
    drained.queue.take() match {
      case KeypadEvent(key, pressed) if pressed => (drained.update(key, pressed), key)
      case _                                    => waitForKey
    }
  }

}

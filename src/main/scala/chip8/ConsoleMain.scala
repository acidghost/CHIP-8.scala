package chip8

import java.awt.event.{KeyEvent, KeyListener}
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.{BlockingQueue, LinkedBlockingQueue}
import javax.swing.{JFrame, SwingUtilities, SwingWorker}

import cats.syntax.flatMap._
import chip8.Concurrency.Producer
import chip8.Cpu.{Failure, Success}
import chip8.Keypad.KeypadEvent

import scala.annotation.tailrec


object ConsoleMain {

  import Keypad.ToKey._

  private[ConsoleMain] case class KeyboardProducer(queue: BlockingQueue[KeypadEvent])
    extends Producer(queue) with KeyListener
  {
    private def updateQueue(keyEvent: KeyEvent, f: Keypad.Key => KeypadEvent): Unit =
      keyEvent.getKeyChar.toKey foreach (f andThen produce)

    override def keyPressed(keyEvent: KeyEvent): Unit  = updateQueue(keyEvent, KeypadEvent(_, pressed = true))
    override def keyTyped(keyEvent: KeyEvent): Unit    = updateQueue(keyEvent, KeypadEvent(_, pressed = true))
    override def keyReleased(keyEvent: KeyEvent): Unit = updateQueue(keyEvent, KeypadEvent(_, pressed = false))
  }

  val keypadQueue = new LinkedBlockingQueue[KeypadEvent]()
  val interrupt = new AtomicBoolean(false)

  val kbFrameRunnable: Runnable = () => new JFrame {
    addKeyListener(KeyboardProducer(keypadQueue))
    setVisible(true)
    new SwingWorker[Unit, Unit] {
      override def doInBackground(): Unit = {
        @tailrec def loop(): Unit = if (interrupt.get()) dispose() else { Thread.sleep(50); loop() }
        loop()
      }
    }.execute()
  }

  def main(args: Array[String]): Unit = {
    (if (args.length < 1)
      Failure("No program supplied")
    else {
      SwingUtilities.invokeAndWait(kbFrameRunnable)
      Cpu(args(0), keypadQueue) flatMap loopWithConsoleOut
    }) match {
      case Success(_) => println("Exited successfully")
      case Failure(e) => println(s"Exited with error: $e")
    }

    interrupt.set(true)
    println(keypadQueue.peek())
  }

  def loopWithConsoleOut(cpu: Cpu) = Cpu.loop(cpu)(cpu => println(cpu.display))

}
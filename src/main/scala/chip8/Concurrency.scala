package chip8

import java.util.concurrent.BlockingQueue

import scala.collection.JavaConverters._


object Concurrency {

  abstract class Producer[T](queue: BlockingQueue[T]) {
    final protected def produce(t: T): Unit = queue put t
  }

  trait QueueDrainer[T] {
    protected def queue: BlockingQueue[T]
    protected def drainQueue[U, S](init: U)(f: (U, T) => U)(g: U => S): S = {
      val list = new java.util.ArrayList[T]()
      if (queue.drainTo(list) > 0)    g(list.asScala.foldLeft(init)(f))
      else                            g(init)
    }
  }

  abstract class Consumer[T](queue: BlockingQueue[T])

}

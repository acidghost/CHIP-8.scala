package chip8

import java.nio.file.{FileSystems, Files}
import java.util.concurrent.BlockingQueue

import cats._
import cats.instances.all._
import cats.syntax.all._
import chip8.Cpu.CpuState
import chip8.Keypad.KeypadEvent
import chip8.Keypad.ToKey._
import chip8.Registers.Register
import chip8.unsigned.{UByte, UShort}

import scala.Function.const
import scala.annotation.tailrec
import scala.language.implicitConversions
import scala.util.{Random, Try}


object Cpu {

  sealed abstract class StepResult[+A]
  final case class Success[+A](value: A) extends StepResult[A]
  final case class Failure(error: String) extends StepResult[Nothing]

  implicit val stepResultMonad = new MonadError[StepResult, String] {
    override def pure[A](x: A): StepResult[A] = Success(x)

    override def flatMap[A, B](fa: StepResult[A])(f: A => StepResult[B]): StepResult[B] = fa match {
      case Success(a) => f(a)
      case fail: Failure => fail
    }

    @tailrec def tailRecM[A, B](a: A)(f: A => StepResult[Either[A, B]]): StepResult[B] = f(a) match {
      case Success(Left(la)) => tailRecM(la)(f)
      case Success(Right(rb)) => pure(rb)
      case fail: Failure => fail
    }

    override def raiseError[A](e: String): StepResult[A] = Failure(e)

    override def handleErrorWith[A](fa: StepResult[A])(f: String => StepResult[A]): StepResult[A] = fa match {
      case success @ Success(_) => success
      case Failure(e) => f(e)
    }
  }

  def failure[A](e: String): StepResult[A] = Failure(e)

  private implicit def stateToSuccess[A](state: A): StepResult[A] = Success(state)

  private implicit class OptOps[A](opt: Option[A]) {
    def toResult(error: => String) = opt match {
      case Some(state) => Success(state)
      case None => Failure(error)
    }
  }

  def apply(programName: String, keypadQueue: BlockingQueue[KeypadEvent]): StepResult[Cpu] =
    CpuState.initial(programName, keypadQueue) map (new Cpu()(_))

  def loop(cpu: Cpu)(f: Cpu => Unit): StepResult[Cpu] =
    cpu.iterateUntilM(_.tick()) { cpu =>
      Thread.sleep(2)
      if (!cpu.halted) f andThen const(false) apply cpu else true
    }

  private case class CpuState(
    programName: String,
    pc: UShort,
    I: UShort,
    registers: Registers,
    stack: Stack,
    memory: Memory,
    display: Display,
    keypad: Keypad,
    delayTimer: UShort,
    soundTimer: UShort,
    halted: Boolean)
  {
    def halt = copy(halted = true)
    def mapPC(f: UShort => UShort) = copy(pc = f(pc))
    def withPC(newPC: UShort) = copy(pc = newPC)
    def next = copy(pc = pc + UShort(2))
    def withRegisters(registers: Registers) = copy(registers = registers)
    def withKeypad(keypad: Keypad) = copy(keypad = keypad)
    private def updateTimer(timer: UShort) = if (timer > UShort.MinValue) timer - UShort(1) else timer
    def tickTimers = copy(delayTimer = updateTimer(delayTimer), soundTimer = updateTimer(soundTimer))
    lazy val opcode = (memory(pc).toUShort << 8) | memory(pc + UShort(1)).toUShort
    lazy val opcodeHex = "0x" + opcode.toHexString.toUpperCase
    lazy val pcHex = "0x" + pc.toHexString.toUpperCase
  }

  private object CpuState {

    def initial(programName: String, keypadQueue: BlockingQueue[KeypadEvent]): StepResult[CpuState] = {
      val path = FileSystems.getDefault.getPath(".", "games", programName.trim)
      Try(Files.readAllBytes(path)).toEither match {
        case Left(e) =>
          Failure(s"Error reading $path: $e")
        case Right(bytes) =>
          CpuState(
            programName = programName,
            pc = UShort(startAddress),
            I = UShort(0),
            registers = Registers.empty,
            stack = Stack.empty,
            memory = Memory.load(bytes),
            display = Display.empty,
            keypad = Keypad.empty(keypadQueue),
            delayTimer = UShort.MinValue,
            soundTimer = UShort.MinValue,
            halted = false)
      }
    }
  }

  private sealed trait Operand[+A] {
    protected def state: CpuState
    def resolve: Option[A]
    def get: StepResult[A] = resolve.toResult(s"Failed to resolve $this in ${state.opcodeHex} @ ${state.pcHex}")
  }

  private final case class RegisterSym(number: UShort)(implicit override val state: CpuState) extends Operand[Register] {
    override def resolve: Option[Register] = Registers fromNum number.toShort
  }

  private final case class RegisterVal(number: UShort)(implicit override val state: CpuState) extends Operand[UByte] {
    override def resolve: Option[UByte] = RegisterSym(number).resolve map (state.registers(_))
  }

  private final case class Immediate(value: UShort)(implicit override val state: CpuState) extends Operand[UShort] {
    override lazy val resolve: Option[UShort] = Some(value)
  }


  private def binOp[A, B](op1: Operand[A], op2: Operand[B])(f: (A, B) => CpuState): StepResult[CpuState] =
    op1.get.flatMap(o1 => op2.get.flatMap(o2 => f(o1, o2)))

  private def skipIf[A, B](op1: Operand[A], op2: Operand[B])(f: (A, B) => Boolean)
                    (implicit state: CpuState): StepResult[CpuState] =
    binOp(op1, op2)((v1, v2) => if (f(v1, v2)) state.next.next else state.next)

  private def updateRegisterOp[A, B](reg: Register, a: A, op: (UByte, B) => UByte, storeVF: Boolean)(g: A => B)
                                    (implicit state: CpuState): CpuState =
    state.withRegisters(state.registers(if (storeVF) Registers.VF else reg) = op(state.registers(reg), g(a)))

  private def registersOp(op: (UByte, UByte) => UByte, storeVF: Boolean = false)(regX: Register, regY: Register)
                         (implicit state: CpuState): CpuState =
    updateRegisterOp(regX, regY, op, storeVF)(state.registers.apply)

  private def registerImmOp(op: (UByte, UByte) => UByte, storeVF: Boolean = false)(reg: Register, imm: UShort)
                           (implicit state: CpuState): CpuState =
    updateRegisterOp(reg, imm, op, storeVF)(_.toUByte)

  private def registerVFOp(op: (UByte, UByte) => Boolean)(regX: Register, regY: Register)
                          (implicit state: CpuState): CpuState =
    registersOp((x, y) => if (op(x, y)) UByte(1) else UByte(0), storeVF = true)(regX, regY)(state)

  private def chainOp[A, B](op: Operand[A], f: A => StepResult[B], g: B => CpuState): StepResult[CpuState] =
    op.get.flatMap(f).map(g)

  private def skipIfKey(op: Operand[Register], g: Boolean => Boolean)(implicit state: CpuState): StepResult[CpuState] =
    chainOp[Register, Keypad.Key](op,
      reg => state.registers(reg).toKey.toResult(s"Failed to parse key from $reg in ${state.opcodeHex}"),
      key => if (g(state.keypad.pressed(key))) state.next.next else state.next)

}


class Cpu private(speed: Int = 700)(implicit private val state: CpuState) {

  import chip8.Cpu._

  val halted = state.halted

  private lazy val opNNN = state.opcode & UShort(0xFFF)
  private lazy val opNN  = state.opcode & UShort(0xFF)
  private lazy val opX   = state.opcode & UShort(0xF00) >> 8
  private lazy val opY   = state.opcode & UShort(0xF0) >> 4
  private lazy val opN   = state.opcode & UShort(0xF)

	def display: Display = state.display

  def tick(): StepResult[Cpu] = {
    println(s"Got opcode ${state.opcodeHex} @ ${state.pcHex} (${state.keypad})")
    val nextState = state.opcode.signed & 0xF000 match {
      case 0x0000                           => op0xxx
      case 0x1000                           => op1xxx
      case 0x2000                           => op2xxx
      case 0x3000                           => op3xxx
      case 0x4000                           => op4xxx
      case 0x5000 if opN == UShort(0x0)     => op5xxx
      case 0x6000                           => op6xxx
      case 0x7000                           => op7xxx
      case 0x8000                           => op8xxx
      case 0x9000 if opN == UShort(0x0)     => op9xxx
      case 0xA000                           => opAxxx
      case 0xB000                           => opBxxx
      case 0xC000                           => opCxxx
      case 0xD000                           => opDxxx
      case 0xE000                           => opExxx
      case 0xF000                           => opFxxx
      case _                                => opUnimplemented
    }

    nextState map (s => new Cpu(speed)(s.tickTimers.withKeypad(state.keypad.withDrainedQ)))
  }

  private def opUnimplemented =
    Failure(s"Unimplemented opcode ${state.opcodeHex} @ ${state.pc} (${state.pc - UShort(startAddress)})")

  private def op0xxx: StepResult[CpuState] = opNNN.signed match {
    case 0x00E =>
      state.copy(display = state.display.clear).next
    case 0x0EE =>
      state.stack.pop.map {
        case (newPC, stack) => state.copy(pc = newPC + UShort(2), stack = stack)
      }.toResult("Failed to return from subroutine: empty stack")
    case _ =>
      state.halt
  }

  private def op1xxx: StepResult[CpuState] = state withPC opNNN

  private def op2xxx: StepResult[CpuState] =
    state.copy(pc = opNNN, stack = state.stack.push(state.pc))

  private def op3xxx: StepResult[CpuState] =
    skipIf(RegisterVal(opX), Immediate(opNN))((r, imm) => r == imm.toUByte)

  private def op4xxx: StepResult[CpuState] =
    skipIf(RegisterVal(opX), Immediate(opNN))((r, imm) => r != imm.toUByte)

  private def op5xxx: StepResult[CpuState] =
    skipIf(RegisterVal(opX), RegisterVal(opY))(_ == _)

  private def op9xxx: StepResult[CpuState] =
    skipIf(RegisterVal(opX), RegisterVal(opY))(_ != _)

  private def op6xxx: StepResult[CpuState] =
    binOp(RegisterSym(opX), Immediate(opNN))(registerImmOp((_, imm) => imm)(_, _).next)

  private def op7xxx: StepResult[CpuState] =
    binOp(RegisterSym(opX), Immediate(opNN))(registerImmOp(_ + _)(_, _).next)

  private def op8xxx: StepResult[CpuState] = opN.signed match {
    case 0x0 =>
      binOp(RegisterSym(opX), RegisterSym(opY))(registersOp((_, valY) => valY)(_, _).next)
    case 0x1 =>
      binOp(RegisterSym(opX), RegisterSym(opY))(registersOp(_ | _)(_, _).next)
    case 0x2 =>
      binOp(RegisterSym(opX), RegisterSym(opY))(registersOp(_ & _)(_, _).next)
    case 0x3 =>
      binOp(RegisterSym(opX), RegisterSym(opY))(registersOp(_ ^ _)(_, _).next)
    case 0x4 =>
      binOp(RegisterSym(opX), RegisterSym(opY)) { (regX, regY) =>
        (registersOp(_ + _)(regX, regY)(_: CpuState)) andThen
          (registerVFOp(_ < _)(regX, regY)(_).next) apply state
      }
    case 0x5 =>
      binOp(RegisterSym(opX), RegisterSym(opY)) { (regX, regY) =>
        (registerVFOp((x, y) => y > x)(regX, regY)(_: CpuState)) andThen
          (registersOp(_ - _)(regX, regY)(_).next) apply state
      }
    case 0x6 =>
      binOp(RegisterSym(opX), RegisterSym(opY)) { (regX, regY) =>
        (registerVFOp((_, y) => (y & UByte(1)) == UByte(1))(regX, regY)(_: CpuState)) andThen
          (registersOp((_, y) => y >>> 1)(regX, regY)(_).next) apply state
      }
    case 0x7 =>
      binOp(RegisterSym(opX), RegisterSym(opY)) { (regX, regY) =>
        (registerVFOp(_ > _)(regX, regY)(_: CpuState)) andThen
          (registersOp((x, y) => y - x)(regX, regY)(_).next) apply state
      }
    case 0xE =>
      binOp(RegisterSym(opX), RegisterSym(opY)) { (regX, regY) =>
        (registerVFOp((_, y) => ((y >>> 7) & UByte(1)) == UByte(1))(regX, regY)(_: CpuState)) andThen
          (registersOp((_, y) => y << 1)(regX, regY)(_).next) apply state
      }
    case _ => opUnimplemented
  }

  //noinspection SpellCheckingInspection
  private def opAxxx: StepResult[CpuState] = state.copy(I = opNNN).next

  //noinspection SpellCheckingInspection
  private def opBxxx: StepResult[CpuState] = state withPC (state.registers(Registers.V0).toUShort + opNNN)

  //noinspection SpellCheckingInspection
  private def opCxxx: StepResult[CpuState] =
    binOp(RegisterSym(opX), Immediate(opNN))(registerImmOp((_, imm) => UByte(Random.nextInt() & imm.signed))(_, _).next)

  //noinspection SpellCheckingInspection
  private def opDxxx: StepResult[CpuState] =
    binOp(RegisterVal(opX), RegisterVal(opY)) { (x, y) =>
      val memSlice = state.memory.slice(state.I.toInt, (state.I + opN).toInt)
      val (collision, display) = state.display.draw(x.toShort, y.toShort, memSlice)
      state.copy(display = display, registers = state.registers.update(Registers.VF, collision)).next
    }

  //noinspection SpellCheckingInspection
  private def opExxx: StepResult[CpuState] = opNN.signed match {
    case 0x9E => skipIfKey(RegisterSym(opX), identity)
    case 0xA1 => skipIfKey(RegisterSym(opX), _.unary_!)
    case _ =>    opUnimplemented
  }

  //noinspection SpellCheckingInspection
  private def opFxxx: StepResult[CpuState] = opNN.signed match {
    case 0x07 =>
      RegisterSym(opX).get map (registerImmOp((_, timer) => timer)(_, state.delayTimer).next)
    case 0x0A =>
      RegisterSym(opX).get map { reg =>
        val (kp, key) = state.keypad.waitForKey
        registerImmOp((_, x) => x)(reg, key.index.toUShort).withKeypad(kp).next
      }
    case 0x15 =>
      RegisterVal(opX).get map (x => state.copy(delayTimer = x.toUShort).next)
    case 0x18 =>
      RegisterVal(opX).get map (x => state.copy(soundTimer = x.toUShort).next)
    case 0x1E =>
      RegisterVal(opX).get map (x => state.copy(I = state.I + x.toUShort).next)
    case 0x29 =>
      RegisterVal(opX).get flatMap { x =>
        if (x.signed < 0 || x.signed > 15)
          Failure(s"Register value not a valid sprite ${x.toHexString} in ${state.opcodeHex} @ ${state.pcHex}")
        else
          Success(state.copy(I = x.toUShort * UShort(5)).next)
      }
    case 0x33 =>
      // TODO: refactor the following 3
      RegisterVal(opX).get map { x =>
        List(x.toInt / 100, x.toInt / 10 % 10, x.toInt % 10).zipWithIndex.foldLeft(state) {
          case (s, (b, i)) => s.copy(memory = s.memory.update(s.I.toInt + i, b.toUByte))
        }.next
      }
    case 0x55 =>
      (0 to opX.toInt).toList.map(i => RegisterVal(i.toUShort).get).sequence[StepResult, UByte].map { values =>
        values.zipWithIndex.foldLeft(state) {
          case (s, (b, i)) => s.copy(memory = s.memory.update(s.I.toInt + i, b))
        }.copy(I = state.I + opX + UShort(1)).next
      }
    case 0x65 =>
      (0 to opX.toInt).toList.map(i => RegisterSym(i.toUShort).get).sequence[StepResult, Register].map { values =>
        values.zipWithIndex.foldLeft(state) {
          case (s, (reg, i)) => registerImmOp((_, mem) => mem)(reg, s.memory(s.I.toInt + i).toUShort)
        }.copy(I = state.I + opX + UShort(1)).next
      }
    case _ => opUnimplemented
  }

}

package cordic

import chisel3._
import chisel3.experimental.FixedPoint
import chisel3.util.Decoupled

import dsptools.numbers._

/**
 * Base class for CORDIC parameters
 *
 * These are type generic
 */
trait CordicParams[T <: Data] {
  val protoXY: T
  val protoZ: T
  val nStages: Int
  val correctGain: Boolean
  val stagesPerCycle: Int
}

/**
 * CORDIC parameters object for fixed-point CORDICs
 */
case class FixedCordicParams(
  // width of X and Y
  xyWidth: Int,
  // width of Z
  zWidth: Int,
  // scale output by correction factor?
  correctGain: Boolean = true,
  // number of CORDIC stages to perform per clock cycle
  stagesPerCycle: Int = 1,
) extends CordicParams[FixedPoint] {
  // prototype for x and y
  // binary point is (xyWidth-2) to represent 1.0 exactly
  val protoXY = FixedPoint(xyWidth.W, (xyWidth-2).BP)
  // prototype for z
  // binary point is (xyWidth-3) to represent Pi/2 exactly
  val protoZ = FixedPoint(zWidth.W, (zWidth-3).BP)
}

/**
 * Bundle type that describes the input, state, and output of CORDIC
 */
class CordicBundle[T <: Data](params: CordicParams[T]) extends Bundle {
  val x: T = params.protoXY.cloneType
  val y: T = params.protoXY.cloneType
  val z: T = params.protoZ.cloneType

  override def cloneType: this.type = CordicBundle(params).asInstanceOf[this.type]
}
object CordicBundle {
  def apply[T <: Data](params: CordicParams[T]): CordicBundle[T] = new CordicBundle(params)
}

/**
 * Bundle type as IO for iterative CORDIC modules
 */
class IterativeCordicIO[T <: Data](params: CordicParams[T]) extends Bundle {
  val in = Flipped(Decoupled(CordicBundle(params)))
  val out = Decoupled(CordicBundle(params))

  val vectoring = Input(Bool())

  override def cloneType: this.type = IterativeCordicIO(params).asInstanceOf[this.type]
}
object IterativeCordicIO {
  def apply[T <: Data](params: CordicParams[T]): IterativeCordicIO[T] =
    new IterativeCordicIO(params)
}

object AddSub {
  def apply[T <: Data : Ring](sel: Bool, a: T, b: T): T = {
    Mux(sel, a + b, a - b)
  }
}

class FixedIterativeCordic(val params: CordicParams[FixedPoint]) extends Module {
  val io = IO(IterativeCordicIO(params))

}

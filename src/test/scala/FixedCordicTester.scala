package cordic

import dsptools.DspTester

/**
 * Case class holding information needed to run an individual test
 */
case class XYZ(
  // input x, y and z
  xin: Double,
  yin: Double,
  zin: Double,
  // mode
  vectoring: Boolean,
  // optional outputs
  // if None, then don't check the result
  // if Some(...), check that the result matches
  xout: Option[Double] = None,
  yout: Option[Double] = None,
  zout: Option[Double] = None
)

/**
 * DspTester for FixedIterativeCordic
 *
 * Run each trial in @trials
 */
class FixedCordicTester(c: FixedIterativeCordic, trials: Seq[XYZ]) extends DspTester(c) {
  val maxCyclesWait = 50

  poke(c.io.out.ready, 1)
  poke(c.io.in.valid, 1)

  for (trial <- trials) {
    poke(c.io.in.bits.x, trial.xin)
    poke(c.io.in.bits.y, trial.yin)
    poke(c.io.in.bits.z, trial.zin)
    poke(c.io.vectoring, trial.vectoring)

    // wait until input is accepted
    var cyclesWaiting = 0
    while (!peek(c.io.in.ready) && cyclesWaiting < maxCyclesWait) {
      cyclesWaiting += 1
      expect(cyclesWaiting < maxCyclesWait, "waited for input too long")
      step(1)
    }
    // wait until output is valid
    cyclesWaiting = 0
    while (!peek(c.io.out.valid) && cyclesWaiting < maxCyclesWait) {
      cyclesWaiting += 1
      expect(cyclesWaiting < maxCyclesWait, "waited for output too long")
      step(1)
    }
    // set desired tolerance
    // in this case, it's pretty loose (2 bits)
    // can you get tolerance of 1 bit? 0? what makes the most sense?
    fixTolLSBs.withValue(2) {
      // check every output where we have an expected value
      trial.xout.foreach { x => expect(c.io.out.bits.x, x) }
      trial.yout.foreach { y => expect(c.io.out.bits.y, y) }
      trial.zout.foreach { z => expect(c.io.out.bits.z, z) }
    }
  }
}

/**
 * Convenience function for running tests
 */
object FixedCordicTester {
  def apply(params: FixedCordicParams, trials: Seq[XYZ]): Boolean = {
    chisel3.iotesters.Driver.execute(Array("-fiwv"), () => new FixedIterativeCordic(params)) {
      c => new FixedCordicTester(c, trials)
    }
  }
}

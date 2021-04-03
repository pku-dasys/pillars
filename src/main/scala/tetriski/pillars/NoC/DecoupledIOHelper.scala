package tetriski.pillars.NoC

import chisel3.{Flipped, UInt, Wire, _}
import chisel3.util.DecoupledIO

class DecoupledIOHelper(flipped: Boolean, dataWidth: Int, channelNum: Int) {

  val bits = VecInit((0 until channelNum).map(_ => 0.U(dataWidth.W)))
  val valids = VecInit((0 until channelNum).map(_ =>  false.B))
  val readys = VecInit((0 until channelNum).map(_ =>  false.B))

  def apply(index: Int) = {
    val ret = new DecoupledIOWrapper(bits(index), valids(index), readys(index), flipped)
    ret
  }

  def apply(index: UInt) = {
    val ret = new DecoupledIOWrapper(bits(index), valids(index), readys(index), flipped)
    ret
  }
}

class DecoupledIOWrapper(bitsIn: Bits, validIn: Bool, readyIn: Bool, flipped: Boolean) {
  val bits = bitsIn
  val valid = validIn
  val ready = readyIn

  def <>(decoupledIO: DecoupledIO[UInt]): Unit = {
    if(flipped){
      decoupledIO.valid := valid
      decoupledIO.bits := bits
      ready := decoupledIO.ready
    }else{
      valid := decoupledIO.valid
      bits := decoupledIO.bits
      decoupledIO.ready := ready
    }
  }

}


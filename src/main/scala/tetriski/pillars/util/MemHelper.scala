package tetriski.pillars.util

import chisel3._
import chisel3.util._
import scala.math.{max, min}

object SplitOrConcat extends Enumeration {
  type Mode = Value
  val Normal, Split, Concat = Value

  def apply(dest_width: Int, src_width: Int) = new {

    val mode = min( +1, max( -1, src_width - dest_width)) match {
      case 1 => Split
      case 0 => Normal
      case -1 => Concat
    }
    val factor = mode match {
      case Split =>
        val fac = src_width / dest_width
        assert(fac * dest_width == src_width, s"$fac * $dest_width != $src_width")
        fac
      case Concat =>
        val fac = dest_width / src_width
        assert(fac * src_width == dest_width, s"$fac * $src_width != $dest_width")
        fac
      case Normal =>
        assert(src_width == dest_width, s"$src_width != $dest_width")
        1
    }
  }
}
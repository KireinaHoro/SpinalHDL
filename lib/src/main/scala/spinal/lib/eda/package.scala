package spinal.lib

import spinal.core._
import spinal.lib.eda.xilinx.ConstraintWriter

package object eda {
  implicit class SpinalReportRicher[T <: Component](spinalReport: SpinalReport[T]) {
    def writeConstraints(fileName: String = null): Unit = {
      ConstraintWriter(spinalReport.toplevel, fileName)
    }
  }
}

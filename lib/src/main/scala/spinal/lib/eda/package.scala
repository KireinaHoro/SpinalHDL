package spinal.lib

import spinal.core._

package object eda {
  implicit class SpinalReportRicher[T <: Component](spinalReport: SpinalReport[T]) {
    def writeConstraints(fileName: String = null): Unit = {
      ConstraintWriter(spinalReport.toplevel, fileName)
    }
  }
}

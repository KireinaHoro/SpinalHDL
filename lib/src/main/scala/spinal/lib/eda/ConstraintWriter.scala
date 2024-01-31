package spinal.lib.eda

import spinal.core.internals._
import spinal.core.{crossClockMaxDelay, _}

import java.io.{File, PrintWriter, Writer}
import scala.language.postfixOps

object ConstraintWriter {
  def apply[T <: Component](
                             c: T,
                             filename: String = null,
                             trustSpinalTimings: Boolean = false
                           ): T = {
    val realFilename = Option(filename).getOrElse(
      f"${GlobalData.get.phaseContext.config.targetDirectory}/${c.getClass.getSimpleName}.tcl"
    )
    val writer = new PrintWriter(new File(realFilename))
    writer.write(
      s"""
         |proc get_driver_cells {dest_pins} {
         |  set net [get_nets -segments -of_objects $$dest_pins]
         |  set source_pins [get_pins -of_objects $$net -filter {IS_LEAF && DIRECTION == OUT}]
         |  return [get_cells -of_objects $$source_pins]
         |}
         |
         |""".stripMargin)
    c.walkComponents(cc => cc.dslBody.walkStatements(doWalkStatements(_, writer)))
    writer.close()
    c
  }

  def doWalkStatements(s: Statement, writer: Writer): Unit = {
    s match {
      case da: DataAssignmentStatement =>
        da.target match {
          case str: SpinalTagReady if str.hasTag(crossClockFalsePath) => writeFalsePath(da, writer)
          case str: SpinalTagReady if str.hasTag(classOf[crossClockMaxDelay]) =>
            writeMaxDelay(da, str.getTag(classOf[crossClockMaxDelay]).get, writer)
          case _ =>
        }
      case _ =>
    }
  }

  // see https://docs.xilinx.com/r/en-US/ug835-vivado-tcl-commands/set_false_path
  def writeFalsePath(s: DataAssignmentStatement, writer: Writer): Unit = {
    val source = s.source.asInstanceOf[BaseType]
    val target = s.target.asInstanceOf[BaseType]
    // TODO trace source to previous FF or input pin
    // TODO fix constraint to find pin
    writer.write(s"""
                    |# CDC constaints for ${s.source.toString} -> ${s.target.toString} in ${s.component.getPath()}
                    |# source: ${s.locationString}
                    |set_false_path -from [get_driver_cells ${source.getRtlPath()}] -to [get_pins ${target.getRtlPath()}_reg*/D]
                    |""".stripMargin)

  }

  // see https://docs.xilinx.com/r/en-US/ug835-vivado-tcl-commands/set_max_delay
  // and https://docs.xilinx.com/r/en-US/ug835-vivado-tcl-commands/set_bus_skew
  def writeMaxDelay(s: DataAssignmentStatement, tag: crossClockMaxDelay, writer: Writer): Unit = {
    val source = s.source.asInstanceOf[BaseType]
    val target = s.target.asInstanceOf[BaseType]
    // TODO trace source to previous FF
    // TODO fix constraint to find pin
    writer.write(s"""
                    |# CDC constraints for ${s.source.toString} -> ${s.target.toString} in ${s.component.getPath()}
                    |# source: ${s.locationString}
                    |set src_clk [get_clocks -quiet -of [get_ports ${source.component.getRtlPath() + "/" + source.clockDomain.clock.getName()}]]
                    |set dst_clk [get_clocks -quiet -of [get_ports ${target.component.getRtlPath() + "/" + target.clockDomain.clock.getName()}]]
                    |set src_clk_period [get_property -quiet -min PERIOD $$src_clk]
                    |set dst_clk_period [get_property -quiet -min PERIOD $$dst_clk]
                    |if {$$src_clk == ""} {
                    |  set src_clk_period 1000
                    |}
                    |if {$$dst_clk == ""} {
                    |  set dst_clk_period 1001
                    |}
                    |
                    |if {($$src_clk != $$dst_clk) || ($$src_clk == "" && $$dst_clk == "")} {
                    |  set_max_delay -from [get_driver_cells ${source.getRtlPath()}] -to [get_pins ${target.getRtlPath()}_reg*/D] $$src_clk_period -datapath_only
                    |  set_bus_skew -from [get_driver_cells ${source.getRtlPath()}] -to [get_pins ${target.getRtlPath()}_reg*/D] [expr min ($$src_clk_period, $$dst_clk_period)]
                    |}
                    |# TODO waive warning
                    |""".stripMargin)
  }

  def fullPath(bt: BaseType) = (if (bt.component != null) bt.component.getPath() + "/" else "") + bt.getDisplayName()
}

case class Test() extends Component {

  import spinal.lib._

  val io = new Bundle {
    val i = in port Bool()
    val o = out port Bool()

    val is = slave port Stream(Bits(2 bit))
    val os = master port Stream(Bits(2 bit))

    val ib = in port Bits(3 bit)
    val ob = out port Bits(3 bit)

    val iv = in port Vec(Bits(2 bit), 2)
    val ov = out port Vec(Bits(2 bit), 2)
  }
  val otherCD = ClockDomain.external("someDomain")

  io.o := BufferCC(io.i, inputAttributes = List(crossClockFalsePath))
  io.os <> io.is.ccToggle(ClockDomain.current, otherCD)

  otherCD {
    io.ob := BufferCC(
      io.ib,
      inputAttributes = List(new crossClockMaxDelay(2, useTargetClock = true))
    )

    io.ov := BufferCC(
      io.iv,
      inputAttributes = List(new crossClockMaxDelay(2, useTargetClock = true))
    )
  }
}

object Test extends App {
  SpinalVerilog(ConstraintWriter(Test()))
}
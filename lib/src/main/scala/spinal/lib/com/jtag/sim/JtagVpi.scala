package solsoc

import spinal.core.sim._
import spinal.lib.com.jtag.Jtag

import java.nio.{ByteBuffer, ByteOrder}
import spinal.lib.com.jtag.sim.JtagDriver

import java.io.{DataInputStream, DataOutputStream}
import java.net.ServerSocket

/*
    * This is a JTAG VPI interface for openocd
    * It is used to connect openocd to a JTAG interface over TCP
    * the reference interface implementation is in https://github.com/openocd-org/openocd/blob/master/src/jtag/drivers/jtag_vpi.c
    * Example openocd config:

source [find interface/jtag_vpi.cfg]
debug_level 1
set _CHIPNAME SOC
jtag newtap $_CHIPNAME cpu -irlen 5 -expected-id 0x10002fff
# init the CPU
target create $_CHIPNAME.cpu riscv -chain-position $_CHIPNAME.cpu
init
 */

object JtagVpi {
  def apply(jtag: Jtag, port: Int = 5555, jtagClkPeriod: Long) = fork {
    val driver = JtagDriver(jtag, jtagClkPeriod)
    var inputStream: DataInputStream = null
    var outputStream: DataOutputStream = null
    class SocketThread extends Thread {
      val socket = new ServerSocket(port)

      override def run(): Unit = {
        println("VPI Server started, waiting for connection...")
        while (true) {
          val connection = try {
            socket.accept()
          } catch {
            case e: Exception => return
          }
          //connection.setTcpNoDelay(true)
          outputStream = new DataOutputStream(connection.getOutputStream())
          inputStream = new DataInputStream(connection.getInputStream())
          println("VPI Client connected")
        }
      }
    }
    val server = new SocketThread
    onSimEnd(server.socket.close())
    server.start()

    // read the first message from the client and print it
    val buffer = new Array[Byte](MaxSizeOfVpiCmd)
    while (true) {
      if(inputStream == null)
      {
        sleep(jtagClkPeriod)
      }
      else {
        inputStream.readFully(buffer)

        val vpiCmd = deserializeVpiCmd(buffer)

        vpiCmd match {
          case VpiCmd(Cmds.RESET, _, _, _, _) => {
            driver.doResetTap()
          }
          case VpiCmd(Cmds.TMS_SEQ, bufferOut, _, _, nbBits) => {
            val tmsSeq = bufferOut.map(x => (0 until 8).map(i => (x & (1 << i)) != 0)).flatten.take(nbBits)
            driver.doTmsSeq(tmsSeq)
          }
          case VpiCmd(Cmds.SCAN_CHAIN, bufferOut, bufferIn, length, nbBits) => {
            // bufferOut is a byte array, we need to convert it to a boolean array
            // bufferIn is a byte array, we need to convert it to a boolean array
            // each byte of bufferOut is a sequence of 8 bits (which is why nbBits exists)
            // convert bufferOut to a boolean array
            val tdiSeq = bufferOut.map(x => (0 until 8).map(i => (x & (1 << i)) != 0)).flatten.take(nbBits)
            val tdoSeq = driver.doScanChain(tdiSeq, false)
            // convert tdoSeq to a byte array grouped by 8 bits
            val tdoSeqBytes = tdoSeq.grouped(8).map(_.foldLeft(0)((acc, b) => (acc << 1) | (if (b) 1 else 0))).toArray
            // copy the result to bufferIn
            for (i <- 0 until length) {
              bufferIn(i) = tdoSeqBytes(i).toByte
            }
            // create packet to send to the client, exactly the same as the received packet but with the tdoSeq
            val vpiCmd = VpiCmd(Cmds.SCAN_CHAIN, bufferOut, bufferIn, length, nbBits)
            serialize(buffer, vpiCmd)
            outputStream.write(buffer)
          }
          case VpiCmd(Cmds.SCAN_CHAIN_FLIP_TMS, bufferOut, bufferIn, length, nbBits) => {
            // same as SCAN_CHAIN but with the last TMS set to 1
            val tdiSeq = bufferOut.map(x => (0 until 8).map(i => (x & (1 << i)) != 0)).flatten.take(nbBits)
            val tdoSeq = driver.doScanChain(tdiSeq, true)
            val tdoSeqBytes =
              tdoSeq.grouped(8).map(_.reverse.foldLeft(0)((acc, b) => (acc << 1) | (if (b) 1 else 0))).toArray
            for (i <- 0 until length) {
              bufferIn(i) = tdoSeqBytes(i).toByte
            }
            val vpiCmd = VpiCmd(Cmds.SCAN_CHAIN_FLIP_TMS, bufferOut, bufferIn, length, nbBits)
            serialize(buffer, vpiCmd)
            outputStream.write(buffer)
          }
          case VpiCmd(Cmds.STOP_SIMU, _, _, _, _) => {
            println("Stop simulation")
            simSuccess()
          }
        }
      }
    }
  }

  def deserializeVpiCmd(buffer: Array[Byte]): VpiCmd = {
    val byteBuffer = ByteBuffer.wrap(buffer)
    byteBuffer.order(ByteOrder.LITTLE_ENDIAN)
    val cmd = byteBuffer.getInt match {
      case 0 => Cmds.RESET
      case 1 => Cmds.TMS_SEQ
      case 2 => Cmds.SCAN_CHAIN
      case 3 => Cmds.SCAN_CHAIN_FLIP_TMS
      case 4 => Cmds.STOP_SIMU
    }
    val bufferOut = new Array[Byte](XFERT_MAX_SIZE)
    byteBuffer.get(bufferOut)
    val bufferIn = new Array[Byte](XFERT_MAX_SIZE)
    byteBuffer.get(bufferIn)
    val length = byteBuffer.getInt
    val nbBits = byteBuffer.getInt
    VpiCmd(cmd, bufferOut, bufferIn, length, nbBits)
  }

  def XFERT_MAX_SIZE = 512

  def serialize(buffer: Array[Byte], vpiCmd: VpiCmd) = {
    val byteBuffer = ByteBuffer.wrap(buffer)
    byteBuffer.order(ByteOrder.LITTLE_ENDIAN)
    vpiCmd.cmd match {
      case Cmds.RESET               => byteBuffer.putInt(0)
      case Cmds.TMS_SEQ             => byteBuffer.putInt(1)
      case Cmds.SCAN_CHAIN          => byteBuffer.putInt(2)
      case Cmds.SCAN_CHAIN_FLIP_TMS => byteBuffer.putInt(3)
      case Cmds.STOP_SIMU           => byteBuffer.putInt(4)
    }
    byteBuffer.put(vpiCmd.bufferOut)
    byteBuffer.put(vpiCmd.bufferIn)
    byteBuffer.putInt(vpiCmd.length)
    byteBuffer.putInt(vpiCmd.nbBits)
  }

  def MaxSizeOfVpiCmd = 4 + 2 * XFERT_MAX_SIZE + 4 + 4

  case class VpiCmd(cmd: Cmds.Cmd, bufferOut: Array[Byte], bufferIn: Array[Byte], length: Int, nbBits: Int)
  object Cmds extends Enumeration {
    type Cmd = Value
    val RESET, TMS_SEQ, SCAN_CHAIN, SCAN_CHAIN_FLIP_TMS, STOP_SIMU = Value
  }
}

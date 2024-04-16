package spinal.lib.bus.amba4.axi

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc._

case class Axi4SlaveFactory(bus: Axi4, cmdPipeline: StreamPipe = StreamPipe.M2S) extends BusSlaveFactoryDelayed {
  bus.setCompositeName(this, "bus", true)

  val readHaltRequest = False
  val writeHaltRequest = False

  val writeCmd = bus.writeCmd.unburstify.pipelined(cmdPipeline)
  val writeJoinEvent = StreamJoin.arg(writeCmd, bus.writeData)
  val writeRsp = Stream(Axi4B(bus.config))
  bus.writeRsp << writeRsp.stage()
  when(bus.writeData.last) {
    // backpressure in last beat
    writeJoinEvent.ready := writeRsp.ready && !writeHaltRequest
    writeRsp.valid := writeJoinEvent.fire
  } otherwise {
    // otherwise, stall W channel when writeHaltRequest
    writeJoinEvent.ready := !writeHaltRequest
    writeRsp.valid := False
  }

  val readCmd = bus.ar.unburstify
  var readDataStage = readCmd.pipelined(cmdPipeline)
  val readRsp = Axi4R(bus.config)
  bus.readRsp << readDataStage.haltWhen(readHaltRequest).translateWith(readRsp)

  // only one outstanding request is supported
  if(bus.config.useId) writeRsp.id := writeCmd.id
  if (writeRsp.config.useResp) {
    when(writeErrorFlag) {
      writeRsp.setSLVERR()
    } otherwise {
      writeRsp.setOKAY()
    }
  }
  if (readRsp.config.useResp) {
    when(readErrorFlag) {
      readRsp.setSLVERR()
    } otherwise {
      readRsp.setOKAY()
    }
  }
  readRsp.data := 0
  readRsp.last := readDataStage.last
  if(bus.config.useId) readRsp.id := readDataStage.id

  val writeOccur = writeJoinEvent.fire
  val readOccur = bus.readRsp.fire

  def maskAddress(addr : UInt) = addr & ~U(bus.config.dataWidth/8 -1, bus.config.addressWidth bits)
  private var remapFunc: UInt => UInt = null
  private def remap(addr: UInt) = if (remapFunc != null) remapFunc(addr) else addr
  def remapAddress(mapping: UInt => UInt): this.type = { remapFunc = mapping; this }

  def readAddressMasked = maskAddress(remap(readDataStage.addr))
  def writeAddressMasked = maskAddress(remap(writeCmd.addr))

  override def readAddress(): UInt  = readAddressMasked
  override def writeAddress(): UInt  = writeAddressMasked

  override def writeByteEnable(): Bits = bus.writeData.strb

  override def readHalt(): Unit = readHaltRequest := True
  override def writeHalt(): Unit = writeHaltRequest := True

  override def build(): Unit = {
    super.doNonStopWrite(bus.writeData.data)

    switch(writeAddress()) {
      for ((address, jobs) <- elementsPerAddress ) address match {
        case address : SingleMapping =>
          assert(address.address % (bus.config.dataWidth/8) == 0)
          is(address.address) {
            doMappedWriteElements(jobs, writeJoinEvent.valid, writeOccur, bus.writeData.data)
          }
        case _ =>
      }
    }

    for ((address, jobs) <- elementsPerAddress if !address.isInstanceOf[SingleMapping]) {
      when(address.hit(writeAddress())) {
        doMappedWriteElements(jobs, writeJoinEvent.valid, writeOccur, bus.writeData.data)
      }
    }


    switch(readAddress()) {
      for ((address, jobs) <- elementsPerAddress) address match {
        case address : SingleMapping =>
          assert(address.address % (bus.config.dataWidth/8) == 0)
          is(address.address) {
            doMappedReadElements(jobs, readDataStage.valid, readOccur, readRsp.data)
          }
        case _ =>
      }
    }

    for ((address, jobs) <- elementsPerAddress if !address.isInstanceOf[SingleMapping]) {
      when(address.hit(readAddress())) {
        doMappedReadElements(jobs, readDataStage.valid, readOccur, readRsp.data)
      }
    }
  }

  override def wordAddressInc: Int = busDataWidth / 8

  override def busDataWidth: Int = bus.config.dataWidth
}

/**
 * Copyright (c) 2013 Bernard Leach
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */
package org.leachbj.hsmsim.akka

import java.net.InetSocketAddress

import org.leachbj.hsmsim.commands.ErrorResponse
import org.leachbj.hsmsim.commands.GenerateIBMPinOffsetRequest
import org.leachbj.hsmsim.commands.GenerateIBMPinOffsetResponse
import org.leachbj.hsmsim.commands.GenerateMacRequest
import org.leachbj.hsmsim.commands.GenerateMacResponse
import org.leachbj.hsmsim.commands.GenerateRSAKeySetRequest
import org.leachbj.hsmsim.commands.GenerateRSAKeySetResponse
import org.leachbj.hsmsim.commands.GenerateRandomPinRequest
import org.leachbj.hsmsim.commands.GenerateRandomPinResponse
import org.leachbj.hsmsim.commands.GenerateZpkRequest
import org.leachbj.hsmsim.commands.GenerateZpkResponse
import org.leachbj.hsmsim.commands.HsmMessageEncoding
import org.leachbj.hsmsim.commands.HsmRequest
import org.leachbj.hsmsim.commands.HsmResponse
import org.leachbj.hsmsim.commands.ImportDesKeyRequest
import org.leachbj.hsmsim.commands.ImportDesKeyResponse
import org.leachbj.hsmsim.commands.TranslatePinZpkToAnotherRequest
import org.leachbj.hsmsim.commands.TranslatePinZpkToAnotherResponse
import org.leachbj.hsmsim.commands.TranslatePinZpkToLmkRequest
import org.leachbj.hsmsim.commands.TranslatePinZpkToLmkResponse
import org.leachbj.hsmsim.commands.TranslateZpkFromZmkToLmkRequest
import org.leachbj.hsmsim.commands.TranslateZpkFromZmkToLmkResponse
import org.leachbj.hsmsim.commands.UnknownHsmRequest
import org.leachbj.hsmsim.commands.VerifyInterchangePinIBMRequest
import org.leachbj.hsmsim.commands.VerifyInterchangePinIBMResponse

import akka.actor.Actor
import akka.actor.ActorLogging
import akka.actor.ActorRef
import akka.actor.Deploy
import akka.actor.Props
import akka.actor.SupervisorStrategy
import akka.actor.actorRef2Scala
import akka.io.BackpressureBuffer
import akka.io.IO
import akka.io.LengthFieldFrame
import akka.io.PipePair
import akka.io.PipelineContext
import akka.io.PipelineStage
import akka.io.Tcp
import akka.io.Tcp.Bind
import akka.io.Tcp.Bound
import akka.io.Tcp.CommandFailed
import akka.io.Tcp.Connected
import akka.io.TcpPipelineHandler
import akka.io.TcpPipelineHandler.Init
import akka.io.TcpPipelineHandler.WithinActorContext
import akka.io.TcpReadWriteAdapter
import akka.util.ByteString

/**
 * The simulator [[Actor]] responsible for listening to connections on port 1501
 * and then creating [[HsmHandler]] [[Actor]] instances to process the client requests.
 */
class HsmSimulator extends Actor with ActorLogging {
  import akka.io.Tcp._
  import context.system

  // there is not recovery for broken connections
  override val supervisorStrategy = SupervisorStrategy.stoppingStrategy

  // bind to the listen port; the port will automatically be closed once this actor dies
  override def preStart(): Unit = {
    IO(Tcp) ! Bind(self, new InetSocketAddress("localhost", 1501))
  }

  // do not restart
  override def postRestart(thr: Throwable): Unit = context stop self

  def receive: Receive = {
    case b @ Bound(localAddress) =>
      log.info("listening on port {}", localAddress.getPort)
      context.become(bound(sender))
    case CommandFailed(Bind(_, local, _, _)) =>
      log.warning(s"cannot bind to [$local]")
      context stop self
  }

  def bound(listener: ActorRef): Receive = {
    case Connected(remote, local) =>
      log.info("received connection from {}", remote)

      val init = TcpPipelineHandler.withLogger(log,
        new HsmMessageStage >>
          new LengthFieldFrame(2048, headerSize = 2, lengthIncludesHeader = false) >>
          new TcpReadWriteAdapter >>
          new BackpressureBuffer(lowBytes = 100, highBytes = 1000, maxBytes = 1000000))

      val connection = sender
      val handler = context.actorOf(Props(new HsmHandler(init)).withDeploy(Deploy.local))
      val pipeline = context.actorOf(TcpPipelineHandler.props(
        init, connection, handler).withDeploy(Deploy.local))

      connection ! Tcp.Register(pipeline)
  }
}

/**
 * Receives [[HsmRequest]] commands and responds with [[HsmResponse]] responses back to the sender.
 */
class HsmHandler(init: Init[WithinActorContext, HsmResponse, HsmRequest]) extends Actor with ActorLogging {
  def receive: Receive = {
    case init.Event(default) =>
      log.debug("Received command: {}", default)
      val processor = context.actorOf(Props(new RequestProcessor(init)))
      processor.forward(default)
    case _ =>
      log.error("Unhandled message!")
  }
}

class RequestProcessor(init: Init[WithinActorContext, HsmResponse, HsmRequest]) extends Actor with ActorLogging {
  def receive: Receive = {
    case t: TranslatePinZpkToAnotherRequest =>
      sender ! init.Command(TranslatePinZpkToAnotherResponse.createResponse(t))
    case i: ImportDesKeyRequest =>
      sender ! init.Command(ImportDesKeyResponse.createResponse(i))
    case g: GenerateRandomPinRequest =>
      sender ! init.Command(GenerateRandomPinResponse("00", "12345"))
    case v: VerifyInterchangePinIBMRequest =>
      sender ! init.Command(VerifyInterchangePinIBMResponse.createResponse(v))
    case translatePin: TranslatePinZpkToLmkRequest =>
      sender ! init.Command(TranslatePinZpkToLmkResponse.createResponse(translatePin))
    case generateOffset: GenerateIBMPinOffsetRequest =>
      sender ! init.Command(GenerateIBMPinOffsetResponse.createResponse(generateOffset))
    case translateZpk: TranslateZpkFromZmkToLmkRequest =>
      sender ! init.Command(TranslateZpkFromZmkToLmkResponse.createResponse(translateZpk))
    case generateZpk: GenerateZpkRequest =>
      sender ! init.Command(GenerateZpkResponse.createResponse(generateZpk))
    case generateMac: GenerateMacRequest =>
      sender ! init.Command(GenerateMacResponse.createResponse(generateMac))
    case generateRsa: GenerateRSAKeySetRequest =>
      sender ! init.Command(GenerateRSAKeySetResponse.createResponse(generateRsa))
    case unknown: UnknownHsmRequest =>
      println("Unknown command type " + unknown.cmd)
      val responseCode = "" + unknown.cmd.charAt(0) + (unknown.cmd.charAt(1) + 1)
      sender ! init.Command(ErrorResponse(responseCode, "99"))
    case _ =>
      println("Unhandled message!")
  }
}

/**
 *  HsmMessageStage converts between arriving events (as ByteString) to HsmRequest instances
 * and commands (as HsmResponse) to ByteString
 */
class HsmMessageStage extends PipelineStage[PipelineContext, HsmResponse, ByteString, HsmRequest, ByteString] {
  override def apply(ctx: PipelineContext) = new PipePair[HsmResponse, ByteString, HsmRequest, ByteString] {
    // command arrives as a HsmResponse and is transformed to a ByteString
    override val commandPipeline = { msg: HsmResponse =>
      println(" Encoding " + msg)
      ctx.singleCommand(HsmMessageEncoding.encode(msg))
    }

    // event arrives from TCP as a ByteString and is transformed to the HsmRequest
    override val eventPipeline = { bs: ByteString =>
      println("processing event: " + bs)
      ctx.singleEvent(HsmMessageEncoding.decode(bs))
    }
  }
}

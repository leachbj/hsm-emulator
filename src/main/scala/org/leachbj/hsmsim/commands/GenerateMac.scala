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
package org.leachbj.hsmsim.commands

import org.leachbj.hsmsim.util.HexConverter
import org.leachbj.hsmsim.crypto.DES
import org.leachbj.hsmsim.crypto.LMK
import akka.util.ByteString

case class GenerateMacRequest(blockNumber: Int, keyType: Int, keyLength: Int, macKey: Array[Byte], iv: Option[Array[Byte]], message: Array[Byte]) extends HsmRequest

case class GenerateMacResponse(errorCode: String, mac: Array[Byte]) extends HsmResponse {
  val responseCode = "MT"
}

object GenerateMacResponse {
  private val (onlyBlock, firstBlock, middleBlock, lastBlock) = (0, 1, 2, 3)
  private val (takKeyType, zakKeyType) = (0, 1)
  private val (singleKeyLen, doubleKeyLen) = (0, 1)
  private val (binaryMessage, hexMessage) = (0, 1)

  def createResponse(req: GenerateMacRequest): HsmResponse = {
    if (req.blockNumber != onlyBlock) return ErrorResponse("MT", "05")
    if (req.keyType != takKeyType && req.keyType != zakKeyType) return ErrorResponse("MT", "04")
    if (req.keyLength != doubleKeyLen) return ErrorResponse("MT", "06")

    val macKey = req.keyType match {
      case `takKeyType` =>
        DES.tripleDesDecryptVariant(LMK.lmkVariant("16-17", 0), req.macKey)
      case `zakKeyType` =>
        DES.tripleDesDecryptVariant(LMK.lmkVariant("26-27", 0), req.macKey)
    }

    println("mac key: " + HexConverter.toHex(ByteString(macKey)))
    if (!DES.isParityAdjusted(macKey)) return ErrorResponse("MT", "10")

    GenerateMacResponse("00", DES.mac(macKey, req.message))
  }
}
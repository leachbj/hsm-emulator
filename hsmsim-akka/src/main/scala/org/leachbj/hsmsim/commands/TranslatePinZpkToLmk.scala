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

import org.leachbj.hsmsim.crypto.DES
import org.leachbj.hsmsim.crypto.LMK
import org.leachbj.hsmsim.util.HexConverter

import akka.util.ByteString

case class TranslatePinZpkToLmkRequest(zpk: Array[Byte], pinBlock: Array[Byte], pinBlockFormat: String, accountNumber: String) extends HsmRequest
case class TranslatePinZpkToLmkResponse(errorCode: String, pin: Array[Byte]) extends HsmResponse {
  val responseCode = "JF"
}
object TranslatePinZpkToLmkResponse {
  def createResponse(req: TranslatePinZpkToLmkRequest): HsmResponse = {
    if (req.pinBlockFormat != "05") return ErrorResponse("JF", "23")

    val zpk = DES.tripleDesDecryptVariant(LMK.lmkVariant("06-07", 0), req.zpk)
    val plainPinBlock = DES.tripleDesDecrypt(zpk, req.pinBlock)
    println("plainPinBlock: " + HexConverter.toHex(ByteString(plainPinBlock)))

    val pinBlock = DES.tripleDesEncrypt(LMK.lmkVariant("02-03", 0), plainPinBlock)


//    val t = DES.tripleDesEncrypt(LMK.lmkVariant("02-03", 0), HexConverter.fromHex("6629123327410").toArray)
//    println(HexConverter.toHex(ByteString(t)))

     println("encryptedPinBlock: " + HexConverter.toHex(ByteString(pinBlock)))
    TranslatePinZpkToLmkResponse("00", pinBlock)
  }
}

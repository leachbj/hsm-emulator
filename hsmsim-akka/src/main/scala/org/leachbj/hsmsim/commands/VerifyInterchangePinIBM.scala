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
import org.leachbj.hsmsim.crypto.IBMPinValidation

case class VerifyInterchangePinIBMRequest(zpk: Array[Byte], pvk: Array[Byte], pinBlock: Array[Byte], pinBlockFormat: String, checkLength: Int, accountNumber: String, decimalisation: Array[Byte], pinValidation: String, offset: String) extends HsmRequest
case class VerifyInterchangePinIBMResponse(errorCode:String) extends HsmResponse {
  val responseCode = "EB"
}

object VerifyInterchangePinIBMResponse {
  def createResponse(req: VerifyInterchangePinIBMRequest): HsmResponse = {
    val zpk = DES.tripleDesDecryptVariant(LMK.lmkVariant("06-07", 0), req.zpk)
    val plainPinBlock = DES.tripleDesDecrypt(zpk, req.pinBlock)
    println("plainPinBlock: " + HexConverter.toHex(ByteString(plainPinBlock)))

    val pvk = DES.tripleDesDecryptVariant(LMK.lmkVariant("14-15", 0), req.pvk)
    val validationBlock = IBMPinValidation.validationBlock(req.pinValidation, req.accountNumber)
    val encryptedValidation = DES.tripleDesEncrypt(pvk, HexConverter.fromHex(validationBlock).toArray)

    val naturalPin = IBMPinValidation.digitReplacement(encryptedValidation, req.decimalisation, req.checkLength)

    val derivedPin = IBMPinValidation.derivePinFromOffset(naturalPin, req.offset)
    println("derivedPin" + derivedPin)

    def extractedPin = {
      val pinAsString = HexConverter.toHex(ByteString(plainPinBlock))
      val pinLength = pinAsString.charAt(1) & 0xf
      pinAsString.substring(2, 2 + pinLength)
    }

    println("Extracted: " + extractedPin)

    if (derivedPin == extractedPin) VerifyInterchangePinIBMResponse("00")
    else VerifyInterchangePinIBMResponse("01")
  }
}

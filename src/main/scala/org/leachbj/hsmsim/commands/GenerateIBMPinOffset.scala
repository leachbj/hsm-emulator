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

/**
 * DE: Generate IBM PIN offset from an LMK encrypted pin
 *
 * @constructor create the request
 * @param pvk 16H, 1A+32H or 1A+48H - PVK encrypted under LMK pair 14-15; used to generate the offset.
 * @parm pinUnderLmk LN or LH - The PIN for which an offset is required; encrypted under LMK pair 02-03.
 * @param checkLength 2N - The minimum PIN length.
 * @param accountNumber 12N - The 12 right-most digits of the account number, excluding check digit.
 * @param decimalization 16N - The table for converting hexadecimal values to decimal.
 * @param pinValidation 12A - User-defined data consisting of hexadecimal characters and the character N, which indicates to the HSM where to insert the last 5 digits of the account number.
 *
 */
case class GenerateIBMPinOffsetRequest(pvk: Array[Byte], pinUnderLmk: Array[Byte], checkLength: Int, accountNumber: String, decimalisation: Array[Byte], pinValidation: String) extends HsmRequest {
  override def toString() = {
    "GenerateIBMPinOffsetRequest(" + HexConverter.toHex(ByteString(pvk)) + ", " + HexConverter.toHex(ByteString(pinUnderLmk)) + "," + checkLength + "," + accountNumber + "," + HexConverter.toHex(ByteString(decimalisation)) + "," + pinValidation + ")"
  }
}

/**
 * DF: Generate IBM PIN offset response
 *
 * @constructor create a successful response
 * @param offset 12N - the PIN offset
 */
case class GenerateIBMPinOffsetResponse(offset: String) extends HsmResponse {
  val errorCode = "00"
  val responseCode = "DF"
}

object GenerateIBMPinOffsetResponse {
  def createResponse(req: GenerateIBMPinOffsetRequest): HsmResponse = {
    val pinBlock = DES.tripleDesDecrypt(LMK.lmkVariant("02-03", 0), req.pinUnderLmk)
    println(HexConverter.toHex(ByteString(pinBlock)))

    // pin block format is NPP..P000 where N is pin length and PP..P is the actual PIN
    def extractedPin = {
      val pinAsString = HexConverter.toHex(ByteString(pinBlock))
      val pinLength = pinAsString.charAt(1) & 0xf
      pinAsString.substring(2, 2 + pinLength)
    }

    println("Extracted: " + extractedPin)

    val pvk = DES.tripleDesDecryptVariant(LMK.lmkVariant("14-15", 0), req.pvk)
    val validationBlock = IBMPinValidation.validationBlock(req.pinValidation, req.accountNumber)
    val encryptedValidation = DES.tripleDesEncrypt(pvk, HexConverter.fromHex(validationBlock).toArray)
    val naturalPin = IBMPinValidation.digitReplacement(encryptedValidation, req.decimalisation, req.checkLength)

    val offset = IBMPinValidation.deriveOffsetFromPin(naturalPin, extractedPin)
    GenerateIBMPinOffsetResponse(offset)
  }
}
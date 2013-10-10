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

case class TranslatePinZpkToAnotherRequest(sourceZpk: Array[Byte], destZpk: Array[Byte], maxPinLength: Int, sourcePinBlock: Array[Byte], sourcePinBlockFormat: String, destinationPinBlockFormat: String, accountNumber: String) extends HsmRequest
case class TranslatePinZpkToAnotherResponse(errorCode: String, pinLength: Int, pinBlock: Array[Byte], pinBlockFormat: String) extends HsmResponse {
  val responseCode = "CD"
}

object TranslatePinZpkToAnotherResponse {
  def createResponse(req: TranslatePinZpkToAnotherRequest): HsmResponse = {
    if (req.sourcePinBlockFormat != "05") return ErrorResponse("CD", "23")
    if (req.destinationPinBlockFormat != "01") return ErrorResponse("CD", "23")

    def decrypt(pin: Array[Byte]) = {
      val key = LMK.lmkVariant("06-07", 0)

      val sourceZpk = DES.tripleDesDecryptVariant(key, req.sourceZpk)
      val plainPinBlock = DES.tripleDesDecrypt(sourceZpk, pin)
      println(HexConverter.toHex(ByteString(plainPinBlock)))

      plainPinBlock
    }

    def encrypt(pin: Array[Byte]) = {
      val key = LMK.lmkVariant("06-07", 0)

      val sourceZpk = DES.tripleDesDecryptVariant(key, req.destZpk)
      val plainPinBlock = DES.tripleDesEncrypt(sourceZpk, pin)
      println("plain pin: " + HexConverter.toHex(ByteString(plainPinBlock)))

      plainPinBlock
    }

    def convertPinBlock(pin: Array[Byte], account: String): (Int, Array[Byte]) = {
      var pinAsString = HexConverter.toHex(ByteString(pin)).toCharArray
      val pinLength = pin(0) & 0xf

      // pin block format 01 - ISO95641 - format 0 => '0' || pin length || pin || padding with 'F' then xor with account number
      pinAsString(0) = '0'

      val paddedAccount = "    " + account;
      for (i <- 2 + pinLength until pinAsString.length) {
        pinAsString(i) = "%X".format(Integer.parseInt("F", 16) ^ Integer.parseInt("" + paddedAccount.charAt(i), 16)).charAt(0)
      }


      println("convertPinBlock: " + new String(pinAsString))

      // pin block format 05 - ISO 956401 Format 1 => '1' || pin length || pin || random padding

//      pinAsString = "0514789FFEFFFC89".toCharArray

      println(new String(pinAsString))

      (pinLength, HexConverter.fromHex(new String(pinAsString)).toArray)
    }

    val oldPinBlock = decrypt(req.sourcePinBlock)
    val (pinLength, newPinBlock) = convertPinBlock(oldPinBlock, req.accountNumber)
    val encryptedNewPinBlock = encrypt(newPinBlock)
    TranslatePinZpkToAnotherResponse("00", pinLength, encryptedNewPinBlock, req.destinationPinBlockFormat)
  }
}

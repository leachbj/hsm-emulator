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

import org.leachbj.hsmsim.crypto.LMK
import org.leachbj.hsmsim.crypto.DES
import org.leachbj.hsmsim.util.HexConverter
import akka.util.ByteString
import java.security.SecureRandom

case class GenerateZpkRequest(zmk: Array[Byte], isAtallaVariant: Boolean, keySchemeZmk: Byte, keySchemeLmk: Byte, keyCheckType: Byte) extends HsmRequest

case class GenerateZpkResponse(errorCode: String, zpkZmk: Array[Byte], zpkLmk: Array[Byte], checkValue: Array[Byte]) extends HsmResponse {
  val responseCode = "IB"
}

object GenerateZpkResponse {
  def createResponse(req: GenerateZpkRequest): HsmResponse = {
    val zmk = DES.tripleDesDecryptVariant(LMK.lmkVariant("04-05", 0), req.zmk)
    val zpk = generateZpk
    val zpkUnderZmk = DES.tripleDesEncrypt(zmk, zpk)
    val zpkUnderLmk = DES.tripleDesEncryptVariant(LMK.lmkVariant("06-07", 0), zpk)
    val checkValue = DES.calculateCheckValue(zpk).take(3)
    GenerateZpkResponse("00", zpkUnderZmk, zpkUnderLmk, checkValue)
  }

  private def generateZpk = {
    val zpk = new Array[Byte](16)
    generator.nextBytes(zpk)
    DES.adjustParity(zpk)
  }

  private val generator = new SecureRandom
}
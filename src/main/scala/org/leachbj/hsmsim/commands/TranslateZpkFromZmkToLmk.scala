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

case class TranslateZpkFromZmkToLmkRequest(zmk: Array[Byte], zpk: Array[Byte], isAtallaVariant: Boolean, keySchemeZmk: Byte, keySchemeLmk: Byte, keyCheckType: Byte) extends HsmRequest

case class TranslateZpkFromZmkToLmkResponse(errorCode: String, zpk: Array[Byte], checkValue: Array[Byte]) extends HsmResponse {
  val responseCode = "FB"
}

object TranslateZpkFromZmkToLmkResponse {
  def createResponse(req: TranslateZpkFromZmkToLmkRequest): HsmResponse = {
    val zmk = DES.tripleDesDecryptVariant(LMK.lmkVariant("04-05", 0), req.zmk)
    val zpk = DES.tripleDesDecrypt(zmk, req.zpk)
    val adjustedZpk = DES.adjustParity(zpk)
    println("zmk: " + HexConverter.toHex(ByteString(zmk)))
    val zpkUnderLmk = DES.tripleDesEncryptVariant(LMK.lmkVariant("06-07", 0), adjustedZpk)
    val checkValue = DES.calculateCheckValue(adjustedZpk).take(3)
    // return 01 if the parity of the ZPK was invalid
    val errorCode = if (zpk == adjustedZpk) "00" else "01"
    TranslateZpkFromZmkToLmkResponse(errorCode, zpkUnderLmk, checkValue)
  }
}
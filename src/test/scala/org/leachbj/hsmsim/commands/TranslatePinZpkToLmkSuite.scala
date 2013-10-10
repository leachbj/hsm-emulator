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

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.leachbj.hsmsim.util.HexConverter
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TranslatePinZpkToLmkSuite extends FunSuite {
  test("translate valid pinblock should return encrypted pin") {
    val zpk = HexConverter.fromHex("971803BA969012152827CC97A98D18D50BE6559CDFF6F7E1").toArray
    val sourcePinBlock = HexConverter.fromHex("F2B66A1070EADC47").toArray
    val accountNumber = "000001000376"
    val req = TranslatePinZpkToLmkRequest(zpk, sourcePinBlock, "05", accountNumber)
    val r = TranslatePinZpkToLmkResponse.createResponse(req)
    assert(r.errorCode === "00")
    assert(r.responseCode === "JF")
    assert(r.isInstanceOf[TranslatePinZpkToLmkResponse])
    val translateResponse = r.asInstanceOf[TranslatePinZpkToLmkResponse]
//    assert(translateResponse.pin === HexConverter.fromHex("6629123327410"))
    
    // this is not the correct value - the LMK encrypted Pin block format is unknown at present
    assert(translateResponse.pin === HexConverter.fromHex("24698C68CF4FA4F9").toArray)
  }
}

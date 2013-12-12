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

import org.scalatest.FunSuite
import scala.collection.immutable.Map
import org.leachbj.hsmsim.util.HexConverter
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TranslatePinZpkToAnotherSuite extends FunSuite {
  test("translate source format 05 to destination format 01") {
    val sourceZpk = HexConverter.fromHex("971803BA969012152827CC97A98D18D50BE6559CDFF6F7E1").toArray
    val destZpk = HexConverter.fromHex("247078713614BE11F819054C5FFB08FF").toArray;
    val maxPinLength = 6
    val sourcePinBlock = HexConverter.fromHex("F2B66A1070EADC47").toArray

    val sourcePinBlockFormat = "05"
    val destinationPinBlockFormat = "01"
    val accountNumber = "000001000376"
    val req = TranslatePinZpkToAnotherRequest(sourceZpk, destZpk, maxPinLength, sourcePinBlock, sourcePinBlockFormat, destinationPinBlockFormat, accountNumber)
    val res = TranslatePinZpkToAnotherResponse.createResponse(req)
    assert(res.errorCode === "00")
    assert(res.responseCode === "CD")
    assert(res.isInstanceOf[TranslatePinZpkToAnotherResponse])
    val translateResponse = res.asInstanceOf[TranslatePinZpkToAnotherResponse]
    assert(translateResponse.pinBlock === HexConverter.fromHex("DBEEBF06C9ED0A48").toArray)
  }
}
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
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.leachbj.hsmsim.util.HexConverter

@RunWith(classOf[JUnitRunner])
class GenerateIBMPinOffsetSuite extends FunSuite {
  test("generate pin offset test vector") {
    val pvk = HexConverter.fromHex("AF9958474101D950930D1FC86F99447E10B3BADFAA10458E").toArray
    // this is not the right value, see TranslatePinZpkToLmkSuite
    val encryptedPin = HexConverter.fromHex("24698C68CF4FA4F9").toArray
    val minLength = 5
    val accountNumber = "000001000376"
    val decimalisation = "0123456789012345".getBytes("UTF-8")
    val pinValidation = "1234567890NF"
    val r = GenerateIBMPinOffsetRequest(pvk, encryptedPin, minLength, accountNumber, decimalisation, pinValidation)
    val resp = GenerateIBMPinOffsetResponse.createResponse(r)
    assert(resp.errorCode === "00")
    assert(resp.responseCode === "DF")
    assert(resp.isInstanceOf[GenerateIBMPinOffsetResponse])
    val generateResponse = resp.asInstanceOf[GenerateIBMPinOffsetResponse]
    assert(generateResponse.offset === "17702")

  }
}
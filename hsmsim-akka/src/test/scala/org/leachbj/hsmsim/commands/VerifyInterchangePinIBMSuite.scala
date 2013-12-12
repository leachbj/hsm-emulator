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
import org.scalatest.junit.JUnitRunner
import org.leachbj.hsmsim.util.HexConverter

@RunWith(classOf[JUnitRunner])
class VerifyInterchangePinIBMSuite extends FunSuite {

//  trait TestValues {
//    val oddParity = HexConverter.fromHex("2A2A2A2A2A2A2A2A").toArray
//    val evenParity = HexConverter.fromHex("2B2B2B2B2B2B2B2B").toArray
//    val key = HexConverter.fromHex("EA040820ABB0CB9E8AF7ADD64F54C73D0289C8923D70FB8C").toArray
//    val encryptedKey = HexConverter.fromHex("971803BA969012152827CC97A98D18D50BE6559CDFF6F7E1").toArray
//    val clearPinBlock = HexConverter.fromHex("1514789d20617088").toArray
//    val encryptedPinBlock = HexConverter.fromHex("F2B66A1070EADC47").toArray
//  }

  test("valid pin data should return 00") {
    val zpk = HexConverter.fromHex("971803BA969012152827CC97A98D18D50BE6559CDFF6F7E1").toArray
    val pvk = HexConverter.fromHex("AF9958474101D950930D1FC86F99447E10B3BADFAA10458E").toArray
    val pinBlock = HexConverter.fromHex("F2B66A1070EADC47").toArray
    val pinBlockFormat = "01"
    val checkLength = 5
    val accountNumber = "000001000376"
    val decimalisation = "0123456789012345".getBytes("UTF-8")
    val pinValidation = "1234567890NF"
    val offset = "17702FFFFFFF"
    val req = VerifyInterchangePinIBMRequest(zpk, pvk, pinBlock, pinBlockFormat, checkLength, accountNumber, decimalisation, pinValidation, offset)
    val r = VerifyInterchangePinIBMResponse.createResponse(req)
    assert(r.errorCode === "00")
    assert(r.responseCode === "EB")
    assert(r.isInstanceOf[VerifyInterchangePinIBMResponse])
  }
}
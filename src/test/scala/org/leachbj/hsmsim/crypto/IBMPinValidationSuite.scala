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
package org.leachbj.hsmsim.crypto

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.leachbj.hsmsim.util.HexConverter

@RunWith(classOf[JUnitRunner])
class IBMPinValidationSuite extends FunSuite {
  import IBMPinValidation._

  trait TestValues {
    val accountNumber = "000001000376"
    val decimalisation = "0123456789012345".getBytes("UTF-8")
    val pinValidation = "1234567890NF"
    val encryptedValue = HexConverter.fromHex("07087E789002BAD5").toArray
    val naturalPin = Vector(48, 55, 48, 56, 55)  // 0 7 0 8 7
    val pin = "14789"
    val offset = "17702"
    val pinLen = 5
  }

  test("validationBlock should correctly insert account number") {
    new TestValues {
      assert(validationBlock(pinValidation, accountNumber) === "123456789000376F")
    }
  }

  test("digitReplacment") {
    new TestValues {
      assert(digitReplacement(encryptedValue, decimalisation, pinLen) === naturalPin)
    }
  }

  test("derivePinFromOffset") {
    new TestValues {
      assert(derivePinFromOffset(naturalPin, offset) === "14789")
    }
  }
  
  test("deriveOffsetFromPin") {
    new TestValues {
      assert(deriveOffsetFromPin(naturalPin, "14789") === offset)
    }
  }
}
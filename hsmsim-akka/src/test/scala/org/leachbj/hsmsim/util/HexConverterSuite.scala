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
package org.leachbj.hsmsim.util

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import akka.util.ByteStringBuilder

@RunWith(classOf[JUnitRunner])
class HexConverterSuite extends FunSuite {
  import org.leachbj.hsmsim.util.HexConverter._

  trait TestValues {
    val byteString = {
      val bs = new ByteStringBuilder
      bs ++= Array[Byte](0x01, 0x23, 0x45, 0x67, 0x89.toByte, 0xAB.toByte, 0xCD.toByte, 0xEF.toByte)
      bs.result
    }
  }
  
  test("toHex should match test vectors") {
    new TestValues {
      assert(toHex(byteString) === "0123456789ABCDEF")
    }
  }
  
  test("toHex should support optional separator") {
    new TestValues {
      assert(toHex(byteString, Option(" ")) == "01 23 45 67 89 AB CD EF")
    }
  }
  
  test("fromHex should match test vectors") {
    new TestValues {
      assert(fromHex("0123456789ABCDEF") === byteString)
    }
  }

  test("fromHex should be case insenstive") {
    new TestValues {
      assert(fromHex("0123456789abcdef") === byteString)
    }
  }
  
  test("fromHex should ignore non-hex characters") {
    new TestValues {
      assert(fromHex("01 2$34z56789/abcd?e@f") === byteString)
    }
  }
}

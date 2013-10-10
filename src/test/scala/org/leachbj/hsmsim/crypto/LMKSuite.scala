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

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.leachbj.hsmsim.util.HexConverter

@RunWith(classOf[JUnitRunner])
class LMKSuite extends FunSuite {
  import org.leachbj.hsmsim.crypto.LMK.lmkVariant
  
  trait TestValues {
    val variant0 = HexConverter.fromHex("20202020202020203131313131313131").toArray
    val variant1 = HexConverter.fromHex("21202020202020203131313131313131").toArray
    val variant2 = HexConverter.fromHex("22202020202020203131313131313131").toArray
    val variant3 = HexConverter.fromHex("23202020202020203131313131313131").toArray
    val variant4 = HexConverter.fromHex("24202020202020203131313131313131").toArray
    val variant5 = HexConverter.fromHex("25202020202020203131313131313131").toArray
    val variant6 = HexConverter.fromHex("26202020202020203131313131313131").toArray
    val variant7 = HexConverter.fromHex("27202020202020203131313131313131").toArray
    val variant8 = HexConverter.fromHex("28202020202020203131313131313131").toArray
  }

  test("lmkVariant 0 test vector") {
    new TestValues {
      assert(lmkVariant("02-03", 0) === variant0)
    }
  }
  test("lmkVariant 1 test vector") {
    new TestValues {
      assert(lmkVariant("02-03", 1) === variant1)
    }
  }
  test("lmkVariant 2 test vector") {
    new TestValues {
      assert(lmkVariant("02-03", 2) === variant2)
    }
  }
  test("lmkVariant 3 test vector") {
    new TestValues {
      assert(lmkVariant("02-03", 3) === variant3)
    }
  }
  test("lmkVariant 4 test vector") {
    new TestValues {
      assert(lmkVariant("02-03", 4) === variant4)
    }
  }
  test("lmkVariant 5 test vector") {
    new TestValues {
      assert(lmkVariant("02-03", 5) === variant5)
    }
  }
  test("lmkVariant 6 test vector") {
    new TestValues {
      assert(lmkVariant("02-03", 6) === variant6)
    }
  }
  test("lmkVariant 7 test vector") {
    new TestValues {
      assert(lmkVariant("02-03", 7) === variant7)
    }
  }
  test("lmkVariant 8 test vector") {
    new TestValues {
      assert(lmkVariant("02-03", 8) === variant8)
    }
  }
  test("lmkVariant 9 is not allowed") {
    intercept[IllegalArgumentException] {
      lmkVariant("02-03", 9)
    }
  }
  test("lmkVariant for unknown pair throws exception") {
    intercept[RuntimeException] {
      lmkVariant("99-03", 1)
    }
  }
}
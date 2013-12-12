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

import org.leachbj.hsmsim.util.HexConverter

object LMK {
  val lmk02_03 = HexConverter.fromHex("20202020202020203131313131313131").toArray
  val lmk04_05 = HexConverter.fromHex("40404040404040405151515151515151").toArray
  val lmk06_07 = HexConverter.fromHex("61616161616161617070707070707070").toArray
  val lmk14_15 = HexConverter.fromHex("E0E0010101010101F1F1010101010101").toArray
  val lmk16_17 = HexConverter.fromHex("1C587F1C13924FEF0101010101010101").toArray
  val lmk26_27 = HexConverter.fromHex("16161616161616161919191919191919").toArray

  val lmk34 = HexConverter.fromHex("2A2A2A2A2A2A2A2A").toArray
  val lmk35 = HexConverter.fromHex("2C2C2C2C2C2C2C2C").toArray

  private val keys = Map("02-03" -> lmk02_03, "04-05" -> lmk04_05, "06-07" -> lmk06_07, "14-15" -> lmk14_15, "16-17" -> lmk16_17, "26-27" -> lmk26_27)
  private val lmkVariants = Array(0x0, 0xa6, 0x5a, 0x6a, 0xde, 0x2b, 0x50, 0x74, 0x9c)

  def lmkVariant(keyName: String, variant: Int) = {
    require(variant < 9)
    def applyLmkVariant(key: Array[Byte]) = {
      val result = key.clone
      result(0) = (result(0) ^ variant.toByte).toByte
      result
    }

    keys.get(keyName) match {
      case None => throw new RuntimeException(s"Unknown LMK $keyName")
      case Some(x) => applyLmkVariant(x)
    }
  }
}

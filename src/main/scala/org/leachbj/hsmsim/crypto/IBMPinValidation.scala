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
import akka.util.ByteString
import java.lang.Math.abs

object IBMPinValidation {
  // validation block is a PREFIX|N|POSTFIX where N is replaced by last 5 digits of account number
  def validationBlock(pinValidation: String, accountNumber: String): String = {
    val Account = "(.*)N(.*)".r
    val Account(prefix, postfix) = pinValidation
    val validation = prefix + accountNumber.takeRight(5) + postfix
    println("validationBlock: " + validation)
    validation
  }

  def digitReplacement(encryptedValidation: Array[Byte], decimalisation: Array[Byte], pinLen: Int) = {
    val block = HexConverter.toHex(ByteString(encryptedValidation))
    val decamlized = block.map(ch =>
      if (ch >= '0' && ch <= '9') ch
      else decimalisation(ch - 'A'))
    println("applyDecimalization: " + decamlized)
    decamlized.take(pinLen)
  }

  private def base10(b: Int): Int = {
    b - '0'
  }

  def derivePinFromOffset(naturalPin: Seq[Int], offset: String) = {
    val d = for (i <- 0 until naturalPin.size) yield ((base10(naturalPin(i)) + base10(offset(i))) % 10)

    d.mkString
  }
  
  def deriveOffsetFromPin(naturalPin: Seq[Int], pin: String) = {
    val d = for (i <- 0 until naturalPin.size) yield (((base10(pin(i)) + 10) - base10(naturalPin(i))) % 10)

    d.mkString
  }
}
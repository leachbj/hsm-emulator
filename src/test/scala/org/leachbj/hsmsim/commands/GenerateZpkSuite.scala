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
import org.leachbj.hsmsim.crypto.DES

@RunWith(classOf[JUnitRunner])
class GenerateZpkSuite extends FunSuite {
  test("generate zpk") {
    val zmkUnderLmk = HexConverter.fromHex("E13D662B185F5F3B08594F89F1FF903A").toArray
    val req = GenerateZpkRequest(zmkUnderLmk, false, 'X', 'U', '0')
    val r = GenerateZpkResponse.createResponse(req)
    assert(r.errorCode === "00")
    assert(r.responseCode === "IB")
    assert(r.isInstanceOf[GenerateZpkResponse])
    val generateResponse = r.asInstanceOf[GenerateZpkResponse]

    // the ZPK is random so decrypt it using the known ZMK and verify the check value
    val zmk = HexConverter.fromHex("8A7C04A1CD916464D6B361B05BBF6780").toArray
    val zpk = DES.tripleDesDecrypt(zmk, generateResponse.zpkZmk)
    val check = DES.calculateCheckValue(zpk).take(3)
    assert(generateResponse.checkValue === check)
  }
}

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
class TranslateZpkFromZmkToLmkSuite extends FunSuite {
  test("translate valid ZPK should match test vector") {
    val zmkUnderLmk = HexConverter.fromHex("E13D662B185F5F3B08594F89F1FF903A").toArray
    val zpkUnderZmk = HexConverter.fromHex("7BC09407A015F72FC59C32147D2AAE57").toArray
    val req = TranslateZpkFromZmkToLmkRequest(/*'U'*/zmkUnderLmk, /*'X'*/zpkUnderZmk, false, 'U', 'U', '0')
    val r = TranslateZpkFromZmkToLmkResponse.createResponse(req)
    assert(r.errorCode === "01")
    assert(r.responseCode === "FB")
    assert(r.isInstanceOf[TranslateZpkFromZmkToLmkResponse])
    val translateResponse = r.asInstanceOf[TranslateZpkFromZmkToLmkResponse]
    
    // verify the first 3 bytes of the checksum
    assert(translateResponse.checkValue === HexConverter.fromHex("DA24FFF7765A").toArray.take(3))
    // key type == 'U'
    assert(translateResponse.zpk === HexConverter.fromHex("0B7C6F68A38FA3C2CB42C2991576A986").toArray)
  }
}

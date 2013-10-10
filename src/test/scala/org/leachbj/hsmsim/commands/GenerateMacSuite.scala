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
import org.leachbj.hsmsim.util.HexConverter
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers

@RunWith(classOf[JUnitRunner])
class GenerateMacSuite extends FunSuite with ShouldMatchers {
  test("generateMac with TAK") {
    val generateReq = GenerateMacRequest(0, 0, 1, HexConverter.fromHex("90A17A99E4BA32475E692D7AAE177BBD").toArray,
      None, "1234567887654321".getBytes)
    val resp = GenerateMacResponse.createResponse(generateReq)
    assert(resp.errorCode === "00")
    assert(resp.responseCode === "MT")
    assert(resp.isInstanceOf[GenerateMacResponse])
    val generateResponse = resp.asInstanceOf[GenerateMacResponse]
    generateResponse.mac should be (HexConverter.fromHex("934F05263310B6D3").toArray)
  }
  
  test("generateMac with ZAK") {
    val generateReq = GenerateMacRequest(0, 1, 1, HexConverter.fromHex("2549A1834EFDDC9629CBD8DC5C982D75").toArray,
      None, "1234567887654321".getBytes)
    val resp = GenerateMacResponse.createResponse(generateReq)
    assert(resp.errorCode === "00")
    assert(resp.responseCode === "MT")
    assert(resp.isInstanceOf[GenerateMacResponse])
    val generateResponse = resp.asInstanceOf[GenerateMacResponse]
    generateResponse.mac should be (HexConverter.fromHex("A1C911EBA50563C9").toArray)
    
  }
}
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
import org.leachbj.hsmsim.util.HexConverter
import akka.util.CompactByteString
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import akka.util.ByteString
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.matchers.HavePropertyMatcher

@RunWith(classOf[JUnitRunner])
class HsmMessageEncodingSuite extends FunSuite with ShouldMatchers {
  //  test("GenerateIBMPinOffset") {
  //    val req = HsmMessageEncoding.decode(ByteString("    DETAF9958474101D950930D1FC86F99447E10B3BADFAA10458E0824698C68CF4FA4F90500000100037601234567890123451234567890NF"))
  //  }
  test("TranslateZpkFromZmkToLmk for non-atalla variant") {
    val req = HsmMessageEncoding.decode(ByteString("    FAUE13D662B185F5F3B08594F89F1FF903AX7BC09407A015F72FC59C32147D2AAE57;UU0"))
    println(req)
    assert(req.isInstanceOf[TranslateZpkFromZmkToLmkRequest])
    val translateReq = req.asInstanceOf[TranslateZpkFromZmkToLmkRequest]
    translateReq.zmk should be(HexConverter.fromHex("E13D662B185F5F3B08594F89F1FF903A").toArray)
    translateReq.zpk should be(HexConverter.fromHex("7BC09407A015F72FC59C32147D2AAE57").toArray)
    translateReq.isAtallaVariant should be(false)
    translateReq.keySchemeLmk should be('U'.toByte)
    translateReq.keySchemeZmk should be('U'.toByte)
    translateReq.keyCheckType should be('0'.toByte)
  }

  test("TranslateZpkFromZmkToLmk for atalla variant with key scheme") {
    val req = HsmMessageEncoding.decode(ByteString("    FAUE13D662B185F5F3B08594F89F1FF903AX7BC09407A015F72FC59C32147D2AAE571;UU0"))
    println(req)
    assert(req.isInstanceOf[TranslateZpkFromZmkToLmkRequest])
    val translateReq = req.asInstanceOf[TranslateZpkFromZmkToLmkRequest]
    translateReq.zmk should be(HexConverter.fromHex("E13D662B185F5F3B08594F89F1FF903A").toArray)
    translateReq.zpk should be(HexConverter.fromHex("7BC09407A015F72FC59C32147D2AAE57").toArray)
    translateReq.isAtallaVariant should be(true)
    translateReq.keySchemeLmk should be('U'.toByte)
    translateReq.keySchemeZmk should be('U'.toByte)
    translateReq.keyCheckType should be('0'.toByte)
  }

  test("TranslateZpkFromZmkToLmk for atalla variant without key scheme") {
    val req = HsmMessageEncoding.decode(ByteString("    FAUE13D662B185F5F3B08594F89F1FF903AX7BC09407A015F72FC59C32147D2AAE571"))
    assert(req.isInstanceOf[TranslateZpkFromZmkToLmkRequest])
    val translateReq = req.asInstanceOf[TranslateZpkFromZmkToLmkRequest]
    translateReq.zmk should be(HexConverter.fromHex("E13D662B185F5F3B08594F89F1FF903A").toArray)
    translateReq.zpk should be(HexConverter.fromHex("7BC09407A015F72FC59C32147D2AAE57").toArray)
    translateReq.isAtallaVariant should be(true)
    translateReq.keySchemeLmk should be('0'.toByte)
    translateReq.keySchemeZmk should be('0'.toByte)
    translateReq.keyCheckType should be('0'.toByte)
  }

  test("TranslateZpkFromZmkToLmk response") {
    val zpk = HexConverter.fromHex("0B7C6F68A38FA3C2CB42C2991576A986").toArray
    val checkValue = HexConverter.fromHex("DA24FF").toArray
    val resp = TranslateZpkFromZmkToLmkResponse("01", zpk, checkValue)
    val encoding = HsmMessageEncoding.encode(resp)
    encoding should be(ByteString("    FB01U0B7C6F68A38FA3C2CB42C2991576A986DA24FF0000000000"))
  }
  
  test("GenerateZpk decode") {
    val req = HsmMessageEncoding.decode(ByteString("    IAUE13D662B185F5F3B08594F89F1FF903A;XU0"))
    assert(req.isInstanceOf[GenerateZpkRequest])
    val generateReq = req.asInstanceOf[GenerateZpkRequest]
    generateReq.zmk should be(HexConverter.fromHex("E13D662B185F5F3B08594F89F1FF903A").toArray)
    generateReq.isAtallaVariant should be(false)
    generateReq.keySchemeLmk should be('U'.toByte)
    generateReq.keySchemeZmk should be('X'.toByte)
    generateReq.keyCheckType should be('0'.toByte)
  }
  
  test("GenerateZpk encode") {
    val zpkUnderZmk = HexConverter.fromHex("1C6FE37B531D49B2D6D5B30D146D6CD6").toArray
    val zpkUnderLmk = HexConverter.fromHex("338120BA5C1B4911DD4CE73F960738DD").toArray
    val checkValue = HexConverter.fromHex("5346A7").toArray
    val resp = GenerateZpkResponse("00", zpkUnderZmk, zpkUnderLmk, checkValue)
    val encoding = HsmMessageEncoding.encode(resp)
    encoding should be(ByteString("    IB00X1C6FE37B531D49B2D6D5B30D146D6CD6U338120BA5C1B4911DD4CE73F960738DD5346A70000000000"))
  }
  
  test("GenerateMac decode TAK") {
    val req = HsmMessageEncoding.decode(ByteString("    MS0010U90A17A99E4BA32475E692D7AAE177BBD00101234567887654321"))
    assert(req.isInstanceOf[GenerateMacRequest])
    val generateReq = req.asInstanceOf[GenerateMacRequest]
    generateReq.blockNumber should be (0)
    generateReq.iv should be (None)
    generateReq.keyLength should be (1)
    generateReq.keyType should be (0)
    generateReq.macKey should be (HexConverter.fromHex("90A17A99E4BA32475E692D7AAE177BBD").toArray)
    generateReq.message should be ("1234567887654321".toArray)
  }
  
  test("GenerateMac decode ZAK") {
    val req = HsmMessageEncoding.decode(ByteString("    MS0111U90A17A99E4BA32475E692D7AAE177BBD001031323334353637383837363534333231"))
    assert(req.isInstanceOf[GenerateMacRequest])
    val generateReq = req.asInstanceOf[GenerateMacRequest]
    generateReq.blockNumber should be (0)
    generateReq.iv should be (None)
    generateReq.keyLength should be (1)
    generateReq.keyType should be (1)
    generateReq.macKey should be (HexConverter.fromHex("90A17A99E4BA32475E692D7AAE177BBD").toArray)
    generateReq.message should be ("1234567887654321".toArray)
  }
  
  test("GenerateRSAKeySet with custom firmware SA command") {
    val req = HsmMessageEncoding.decode(HexConverter.fromHex("2020202053413030353132303130303137010001"))
    assert(req.isInstanceOf[GenerateRSAKeySetRequest])
    val generateReq = req.asInstanceOf[GenerateRSAKeySetRequest]
    generateReq.keyType should be (0)
    generateReq.keyLength should be (512)
    generateReq.publicKeyEncoding should be (1)
    generateReq.publicExponent should be (0x10001)
  }
  
  test("GenerateRSAKeySteResponse for custom SA command") {
    val publicKey = HexConverter.fromHex("30470240DCAC85790D770C4CAA562B4E50DFB42F4619B1AF8E3870714691D08C512DA0E365518D92BCFC5BAC28514C047DA4F61E6CB2B9DDA939C3594B7E60880D0B13770203010001").toArray
    val secretKey = HexConverter.fromHex("60779D7300F8C209B4C443C67367ADECBD80CAE5972843B9BE2990B573903DDE765CD87AA823056AC472674C5AF9A75127FC7E66340E3C2CC27F051406E6803D5556991FFE66A9C3B1288E7FC893C3B3123C955ED8DBD8E9530F1D5FB4CD3E1050BA913F0430C230F596306BEFDB096ABC8B12B2111A5CBF951F879E0AB063368BAA2BDD1BA69E80D285323BFD267B48720B81B33F0B5A23EB5552DE94F3319F953FB508A27CA894FC18EFE7D068E53D").toArray
    val resp = GenerateRSAKeySetResponse(publicKey, secretKey)
    val encoding = HsmMessageEncoding.encode(resp)
    encoding should be (HexConverter.fromHex("202020205342303030470240dcac85790d770c4caa562b4e50dfb42f4619b1af8e3870714691d08c512da0e365518d92bcfc5bac28514c047da4f61e6cb2b9dda939c3594b7e60880d0b137702030100013031373660779d7300f8c209b4c443c67367adecbd80cae5972843b9be2990b573903dde765cd87aa823056ac472674c5af9a75127fc7e66340e3c2cc27f051406e6803d5556991ffe66a9c3b1288e7fc893c3b3123c955ed8dbd8e9530f1d5fb4cd3e1050ba913f0430c230f596306befdb096abc8b12b2111a5cbf951f879e0ab063368baa2bdd1ba69e80d285323bfd267b48720b81b33f0b5a23eb5552de94f3319f953fb508a27ca894fc18efe7d068e53d"))
  }
}
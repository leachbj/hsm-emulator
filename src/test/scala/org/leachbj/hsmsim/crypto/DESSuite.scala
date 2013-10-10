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
import javax.crypto.spec.IvParameterSpec
import javax.crypto.Cipher
import javax.crypto.SecretKeyFactory
import javax.crypto.spec.DESKeySpec
import akka.util.ByteString
import java.security.Security
import org.bouncycastle.jce.provider.BouncyCastleProvider
import javax.crypto.Mac

@RunWith(classOf[JUnitRunner])
class DESSuite extends FunSuite {
  import org.leachbj.hsmsim.crypto.DES._

  trait TestValues {
    val oddParity = HexConverter.fromHex("2A2A2A2A2A2A2A2A").toArray
    val evenParity = HexConverter.fromHex("2B2B2B2B2B2B2B2B").toArray
    val key = HexConverter.fromHex("EA040820ABB0CB9E8AF7ADD64F54C73D0289C8923D70FB8C").toArray
    val encryptedKey = HexConverter.fromHex("971803BA969012152827CC97A98D18D50BE6559CDFF6F7E1").toArray
    val clearPinBlock = HexConverter.fromHex("1514789d20617088").toArray
    val encryptedPinBlock = HexConverter.fromHex("F2B66A1070EADC47").toArray
    val keyDoubleLength = HexConverter.fromHex("F1799DF10D51EA89A849F22F3DDA7A80").toArray
    val encryptedDoubleLength = HexConverter.fromHex("247078713614BE11F819054C5FFB08FF").toArray
  }

  test("mac with nist test vector") {
    val data = "Hello World !!!!".getBytes
    val key = HexConverter.fromHex("7CA110454A1A6E570131D9619DC1376E").toArray
    assert(mac(key, data) === HexConverter.fromHex("F09B856213BAB83B").toArray)
  }

  test("mac with TAK test vector") {
    val key = HexConverter.fromHex("A24A64E9371F523DDA94E5E9CB894F13").toArray
    val data = "1234567887654321".getBytes
    assert(mac(key, data) === HexConverter.fromHex("934F05263310B6D3").toArray)
  }
  
  test("mac with ZAK test vector") {
    val key = HexConverter.fromHex("082FD59BD0F8B9981A2C160E19ABDA25").toArray
    val data = "1234567887654321".getBytes
    assert(mac(key, data) === HexConverter.fromHex("A1C911EBA50563C9").toArray)
  }

  test("isParityAdjusted should return true for oddParity") {
    new TestValues {
      assert(isParityAdjusted(oddParity))
    }
  }

  test("isParityAdjusted should return flase for evenParity") {
    new TestValues {
      assert(isParityAdjusted(evenParity) == false)
    }
  }

  test("adjust parity for odd input return odd parity key") {
    new TestValues {
      assert(adjustParity(oddParity) === oddParity)
    }
  }
  
  test("adjust parity for even input should return odd parity key") {
    new TestValues {
      assert(adjustParity(evenParity) === oddParity)
    }
  }
  
  test("tripleDesEncryptVariant should support triple length keys") {
    new TestValues {
      assert(tripleDesEncryptVariant(LMK.lmkVariant("06-07", 0), key) === encryptedKey)
    }
    
  }
  
  test("tripleDesEncryptVariant should support double length keys") {
    new TestValues {
      assert(tripleDesEncryptVariant(LMK.lmkVariant("06-07", 0), keyDoubleLength) === encryptedDoubleLength)
    }
  }
  
  test("tripleDesDecryptVariant should support triple length keys") {
    new TestValues {
      assert(tripleDesDecryptVariant(LMK.lmkVariant("06-07", 0), encryptedKey) === key)
    }
  }
  
  test("tripleDesDecryptVariant should support double length keys") {
    new TestValues {
      assert(tripleDesDecryptVariant(LMK.lmkVariant("06-07", 0), encryptedDoubleLength) === keyDoubleLength)
    }
  }
  
  test("tripleDesEncrypt test vector") {
    new TestValues {
      assert(tripleDesEncrypt(key, clearPinBlock) === encryptedPinBlock)
    }
  }

  test("tripleDesDecrypt test vector") {
    new TestValues {
      assert(tripleDesDecrypt(key, encryptedPinBlock) === clearPinBlock)
    }
  }

  test("calculateCheckValue test vector") {
    new TestValues {
      assert(calculateCheckValue(key) === HexConverter.fromHex("6EC2A9AE788F").toArray)
    }
  }
}
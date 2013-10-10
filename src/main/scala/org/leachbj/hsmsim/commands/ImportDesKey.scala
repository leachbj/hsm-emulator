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

import java.lang.Math
import java.security.KeyFactory
import java.security.interfaces.RSAPrivateKey
import java.security.spec.RSAPrivateCrtKeySpec

import scala.math.BigInt.int2bigInt

import org.leachbj.hsmsim.crypto.DES
import org.leachbj.hsmsim.crypto.LMK
import org.leachbj.hsmsim.util.HexConverter

import akka.util.ByteString
import javax.crypto.Cipher
import javax.crypto.SecretKeyFactory
import javax.crypto.spec.DESKeySpec
import javax.crypto.spec.IvParameterSpec

case class ImportDesKeyRequest(desKey: Array[Byte], rsaPrivKey: Array[Byte], keySchemeLmk: Byte) extends HsmRequest {
  override def toString() = {
    "ImportDesKeyRequest(" + HexConverter.toHex(ByteString(desKey)) + ", " + HexConverter.toHex(ByteString(rsaPrivKey)) + "," + keySchemeLmk + ")"
  }
}
case class ImportDesKeyResponse(errorCode: String, desKey: Array[Byte], keyCheckValue: Array[Byte]) extends HsmResponse {
  val responseCode = "GJ"
}
object ImportDesKeyResponse {
  private def ceil(x: Int): Int = {
    (Math.ceil(x / 24d)).asInstanceOf[Int] * 3
  }

  def createResponse(req: ImportDesKeyRequest): HsmResponse = {
    def decryptHsmPrivateKey = {
      def reverse(input: Array[Byte]) = {
        require(input.length % 8 == 0, "Input length must be a multiple of 8.")

        // reverse the blocks of the input so that first block is last and last block is first
        input.grouped(8).toArray.reverse.flatten.toArray
      }

      val skf = SecretKeyFactory.getInstance("DES")
      val skey34 = skf.generateSecret(new DESKeySpec(LMK.lmk34))
      val skey35 = skf.generateSecret(new DESKeySpec(LMK.lmk35))
      val cipher = Cipher.getInstance("DES/CBC/NoPadding")
      val zeroIV = new IvParameterSpec(new Array[Byte](8))

      // decrypt with LMK 34 left to right
      cipher.init(Cipher.DECRYPT_MODE, skey34, zeroIV)
      val step6 = cipher.doFinal(req.rsaPrivKey)

      // encrypt with LMK 35 right to left (and reverse the result)
      cipher.init(Cipher.ENCRYPT_MODE, skey35, zeroIV)
      val step7 = reverse(cipher.doFinal(reverse(step6)))

      // decrypt with LMK 34 left to right
      cipher.init(Cipher.DECRYPT_MODE, skey34, zeroIV)
      cipher.doFinal(step7)
    }

    def createRsaKey = {
      val decryptedKey: Array[Byte] = decryptHsmPrivateKey

      println(HexConverter.toHex(ByteString(decryptedKey)))

      val bitLen = (decryptedKey(2) & 0xff) * 256 + (decryptedKey(3) & 0xff)

      println("bitLen = " + bitLen)

      def getComponents = {
        val padLen = ceil(bitLen / 2)

        println("padLen = " + padLen)
        // skip the 4 byte header and then convert each padLen bytes into a BigInt
        val components = decryptedKey.drop(4).grouped(padLen).map(comp => BigInt(1, comp)).toArray

        (components(0), components(1), components(2), components(3), components(4))
      }

      // 4 byte header plus the 5 components
      val data = decryptedKey.take(4 + 5 * ceil(bitLen / 2))
      println(data.length)
      println((data.length + 8 - 1) / 8 * 8)

      // zero pad
      // pad it out to a multiple of 8 with zeroes
      val dataToMac = data.padTo((data.length + 8 - 1) / 8 * 8, 0.toByte)

      {
        val skf = SecretKeyFactory.getInstance("DES")
        val key = LMK.lmk35.clone
        key(0) = (key(0) ^ 0xa6).toByte // variant(0)
        val lmk35 = skf.generateSecret(new DESKeySpec(key))
        val cipher = Cipher.getInstance("DES/CBC/NoPadding")
        val iv = new IvParameterSpec(new Array[Byte](8))

        println(HexConverter.toHex(ByteString(dataToMac)))
        cipher.init(Cipher.ENCRYPT_MODE, lmk35, iv)
        val mac = cipher.doFinal(dataToMac).takeRight(8).take(4)
        println("mac: " + HexConverter.toHex(ByteString(mac)))
        println("org: " + HexConverter.toHex(ByteString(decryptedKey.drop(data.length).take(4))))

        // TODO need to refactor to be able to do this
        //        if (mac.deep != decryptedKey.drop(data.length).take(4).deep) return ErrorResponse("GJ", "84")
        assert(mac.deep == decryptedKey.drop(data.length).take(4).deep, "Invalid mac on decrypted key")
      }

      val (p, q, d1, d2, crt) = getComponents
      val mod = p * q
      val e = d1 modInverse (p - 1)
      List(p, q, d1, d2, crt, mod, e).foreach(b => println(b.toString(16)))
      val d = e modInverse ((p - 1) * (q - 1))
      val rsaPrivateKeySpec = new RSAPrivateCrtKeySpec(mod.bigInteger, e.bigInteger, d.bigInteger, p.bigInteger, q.bigInteger, d1.bigInteger, d2.bigInteger, crt.bigInteger)

      KeyFactory.getInstance("RSA").generatePrivate(rsaPrivateKeySpec).asInstanceOf[RSAPrivateKey]
    }

    val key = createRsaKey

    println(req.desKey.length + " " + (key.getModulus().bitLength() / 8))

    if (req.desKey.length > key.getModulus().bitLength() / 8) {
      println("Imported des key too long")
      return ErrorResponse("GJ", "76")
    }

    def decryptDesKey = {
      val cipher = Cipher.getInstance("RSA/ECB/PKCS1Padding")
      cipher.init(Cipher.DECRYPT_MODE, key)
      try {
    	  Some(cipher.doFinal(req.desKey))
      } catch {
        case e: Exception =>
          e.printStackTrace()
          None
      }
    }

    val d = decryptDesKey
    if (d.isEmpty) return ErrorResponse("GJ", "15")
    val decryptedDesKeyNoParity = d.get

    println("deskey: " + HexConverter.toHex(ByteString(decryptedDesKeyNoParity)))
    //req.keySchemeLmk is 'T'

    val decryptedDesKey = DES.adjustParity(decryptedDesKeyNoParity)
    println("deskey: " + HexConverter.toHex(ByteString(decryptedDesKey)))

    ImportDesKeyResponse("00", DES.tripleDesEncryptVariant(LMK.lmkVariant("06-07", 0), decryptedDesKey), DES.calculateCheckValue(decryptedDesKey).take(6))
  }
}

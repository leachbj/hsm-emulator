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

import java.security.KeyPairGenerator
import java.security.interfaces.RSAPrivateCrtKey
import java.security.interfaces.RSAPublicKey
import java.security.spec.RSAKeyGenParameterSpec
import org.bouncycastle.asn1.x509.RSAPublicKeyStructure
import org.leachbj.hsmsim.crypto.LMK
import org.leachbj.hsmsim.util.HexConverter
import akka.util.ByteString
import javax.crypto.Cipher
import javax.crypto.SecretKeyFactory
import javax.crypto.spec.DESKeySpec
import javax.crypto.spec.IvParameterSpec
import scala.math.BigInt._
import java.math.BigInteger

case class GenerateRSAKeySetRequest(keyType: Int, keyLength: Int, publicKeyEncoding: Int, publicExponent: Int) extends HsmRequest

case class GenerateRSAKeySetResponse(publicKey: Array[Byte], privateKey: Array[Byte]) extends HsmResponse {
  val errorCode = "00"
  val responseCode = "SB"
}

object GenerateRSAKeySetResponse {
  def createResponse(req: GenerateRSAKeySetRequest): HsmResponse = {
    val flags = req.keyType.toByte

    val (pub, priv) = generateRsaKeyPair(req.keyLength, BigInteger.valueOf(req.publicExponent))
    val pubEncoded = encodePublicKey(pub)
    val privEncoded = encodePrivateKey(flags, priv)

    GenerateRSAKeySetResponse(pubEncoded, privEncoded)
  }

  private def generateRsaKeyPair(keyLength: Int, modulus: BigInteger) = {
    val keyPairGenerator = KeyPairGenerator.getInstance("RSA")
    keyPairGenerator.initialize(new RSAKeyGenParameterSpec(keyLength, modulus))
    val keyPair = keyPairGenerator.generateKeyPair()
    (keyPair.getPublic.asInstanceOf[RSAPublicKey], keyPair.getPrivate.asInstanceOf[RSAPrivateCrtKey])
  }

  private def encodePublicKey(pubKey: RSAPublicKey) = {
    println(pubKey.getFormat)
    
    new RSAPublicKeyStructure(pubKey.getModulus(), pubKey.getPublicExponent()).getEncoded()
  }

  private def encodePrivateKey(flags: Byte, privKey: RSAPrivateCrtKey) = {
    def ceil(x: Int): Int = {
      (Math.ceil(x / 24d)).asInstanceOf[Int] * 3
    }

    def padTo8(data: Array[Byte]) = data.padTo((data.length + 8 - 1) / 8 * 8, 0.toByte)

    def macEncodedKey(encodedKey: Array[Byte]) = {
      val skf = SecretKeyFactory.getInstance("DES")
      val key = LMK.lmk35.clone
      key(0) = (key(0) ^ 0xa6).toByte // variant(0)
      val lmk35Variant0 = skf.generateSecret(new DESKeySpec(key))
      val cipher = Cipher.getInstance("DES/CBC/NoPadding")
      val iv = new IvParameterSpec(new Array[Byte](8))

      // pad it out to a multiple of 8 with zeroes
      val dataToMac = padTo8(encodedKey)

      println(HexConverter.toHex(ByteString(dataToMac)))
      cipher.init(Cipher.ENCRYPT_MODE, lmk35Variant0, iv)
      cipher.doFinal(dataToMac).takeRight(8).take(4)
    }

    val bitLength = privKey.getModulus().bitLength()
    println("BitLength: " + bitLength)

    val components = Array(privKey.getPrimeP(), privKey.getPrimeQ(), privKey.getPrimeExponentP(), privKey.getPrimeExponentQ(), privKey.getCrtCoefficient())
    val header = Array(flags, 0.toByte, ((bitLength / 256) & 0xff).toByte, (bitLength & 0xff).toByte)
    val componentLen = ceil(bitLength / 2)
    val encodedKey = components.map(comp => new Array[Byte](componentLen) ++ comp.toByteArray takeRight (componentLen)).flatten
    val mac = macEncodedKey(header ++ encodedKey)
    encryptKey(padTo8(header ++ encodedKey ++ mac))
  }

  def encryptKey(secretKey: Array[Byte]) = {
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
    cipher.init(Cipher.ENCRYPT_MODE, skey34, zeroIV)
    val step6 = cipher.doFinal(secretKey)

    // encrypt with LMK 35 right to left (and reverse the result)
    cipher.init(Cipher.DECRYPT_MODE, skey35, zeroIV)
    val step7 = reverse(cipher.doFinal(reverse(step6)))

    // decrypt with LMK 34 left to right
    cipher.init(Cipher.ENCRYPT_MODE, skey34, zeroIV)
    cipher.doFinal(step7)
  }
}
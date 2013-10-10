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

import scala.Array.canBuildFrom

import org.leachbj.hsmsim.util.HexConverter

import akka.util.ByteString
import javax.crypto.Cipher
import javax.crypto.SecretKeyFactory
import javax.crypto.spec.DESKeySpec
import javax.crypto.spec.DESedeKeySpec
import javax.crypto.spec.IvParameterSpec

object DES {
  private def bitcount(c: Byte) = {
    val s = (c & 0x11) + ((c >> 1) & 0x11) + ((c >> 2) & 0x11) + ((c >> 3) & 0x11)
    (s & 15) + (s >> 4)
  }

  private def isOddBitCount(c: Byte) = bitcount(c) % 2 == 1
  
  def isParityAdjusted(key: Array[Byte]): Boolean = {
    key.forall(isOddBitCount)
  }

  def adjustParity(key: Array[Byte]): Array[Byte] = {
    def setparity(c: Byte) =
      if (isOddBitCount(c)) c
      else (c ^ 1).toByte

    key.map(setparity(_))
  }

  private def applyVariant(key: Array[Byte], variant: Int) = {
    val result = key.clone
    result(8) = (result(8) ^ variant.toByte).toByte
    println("des_v: " + HexConverter.toHex(ByteString(result)))
    result
  }

  private def cipher(mode: Int, key: Array[Byte], data: Array[Byte]) = {
    // Sun DESede provider requires triple DES keys to be 24 bytes
    val k = if (key.length == 16) key ++ key.slice(0, 8) else key

    val skey = SecretKeyFactory.getInstance("DESede").generateSecret(new DESedeKeySpec(k))
    val cipher = Cipher.getInstance("DESede/ECB/NoPadding")
    cipher.init(mode, skey)
    val result = cipher.doFinal(data)
    println("DES(k,v): " + HexConverter.toHex(ByteString(k)) + ", " + HexConverter.toHex(ByteString(data)) + " -> " + HexConverter.toHex(ByteString(result)))
    result
  }

  def tripleDesEncrypt(key: Array[Byte], data: Array[Byte]): Array[Byte] = cipher(Cipher.ENCRYPT_MODE, key, data)

  def tripleDesDecrypt(key: Array[Byte], data: Array[Byte]): Array[Byte] = cipher(Cipher.DECRYPT_MODE, key, data)

  private val doubleVariants = Array(0xa6, 0x5a)
  private val tripleVariants = Array(0x6a, 0xde, 0x2b)

  private type CryptoOp = (Array[Byte], Array[Byte]) => Array[Byte]

  private def tripleDesWithVariant(cryptoOp: CryptoOp, lmkKey: Array[Byte], key: Array[Byte]): Array[Byte] = {
    val variant = if (key.length == 24) tripleVariants else doubleVariants

    val d = for (i <- 0 until (key.length, 8))
      yield cryptoOp(applyVariant(lmkKey, variant(i / 8)), key.slice(i, i + 8))

    d.flatten.toArray
  }

  def tripleDesEncryptVariant(lmkKey: Array[Byte], key: Array[Byte]): Array[Byte] =
    tripleDesWithVariant(tripleDesEncrypt, lmkKey, key)

  def tripleDesDecryptVariant(lmkKey: Array[Byte], key: Array[Byte]): Array[Byte] =
    tripleDesWithVariant(tripleDesDecrypt, lmkKey, key)

  def mac(key: Array[Byte], data: Array[Byte]) = {
    // CBC encrypt the data and discard all but the final block
    val skf = SecretKeyFactory.getInstance("DES")
    val cbcCipher = Cipher.getInstance("DES/CBC/NoPadding")
    val iv = new IvParameterSpec(new Array[Byte](8))
    cbcCipher.init(Cipher.ENCRYPT_MODE, skf.generateSecret(new DESKeySpec(key.take(8))), iv)
    val lastBlock = cbcCipher.doFinal(data).takeRight(8)

    // now decrypt & encrypt the final block with the 2nd and 1st keys respectively
    val cipher = Cipher.getInstance("DES/ECB/NoPadding")
    cipher.init(Cipher.DECRYPT_MODE, skf.generateSecret(new DESKeySpec(key.takeRight(8))))
    val lastBlockDec = cipher.doFinal(lastBlock)
    cipher.init(Cipher.ENCRYPT_MODE, skf.generateSecret(new DESKeySpec(key.take(8))))
    cipher.doFinal(lastBlockDec)
  }

  def calculateCheckValue(key: Array[Byte]) = tripleDesEncrypt(key, new Array[Byte](8)).take(6)
}

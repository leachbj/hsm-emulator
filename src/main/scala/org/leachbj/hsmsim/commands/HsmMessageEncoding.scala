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
 */package org.leachbj.hsmsim.commands

import org.leachbj.hsmsim.util.HexConverter
import akka.util.ByteString

trait HsmRequest

trait HsmResponse {
  val responseCode: String
  val errorCode: String
}

case class UnknownHsmRequest(cmd: String) extends HsmRequest

case class ErrorResponse(responseCode: String, errorCode: String) extends HsmResponse

case class NcHsmResponse(responseCode: String, errorCode: String, lmkCheckValue: String, firmwareVersion: String) extends HsmResponse

case class GenerateRandomPinRequest(accountNumber: String, pinLength: Int) extends HsmRequest
case class GenerateRandomPinResponse(errorCode: String, pin: String) extends HsmResponse {
  val responseCode = "JB"
}

object HsmMessageEncoding {
  def decode(bs: ByteString): HsmRequest = {
    val iter = bs.iterator

    def readStringAsBytes(length: Int) = {
      val bytes = new Array[Byte](length)
      iter getBytes bytes
      bytes
    }

    def readString(length: Int) = ByteString(readStringAsBytes(length)).utf8String

    def readLengthBytes = {
      val length = readNumeric(4)
      val bytes = new Array[Byte](length)
      iter getBytes bytes
      bytes
    }

    def readLmkType = readString(4)

    def readKey = {
      iter.head match {
        case 'U' =>		// double
          iter.drop(1)
          readHex(32)
        case 'X' =>		// double X9.17
          iter.drop(1)
          readHex(32)
        case 'T' =>		// triple
          iter.drop(1)
          readHex(48)
        case 'Y' =>		// double X9.17
          iter.drop(1)
          readHex(48)
        case _ =>
          readHex(16)
      }
    }

    def readNumeric(len: Int) = readString(len).toInt
    
    def readHexNumeric(len: Int) = Integer.parseInt(readString(len), 16)

    def readHex(len: Int) = HexConverter.fromHex(readString(len))

    iter.drop(4) // skip header
    val cmd = readString(2)
    println("got cmd: " + cmd)
    cmd match {
      case "CC" =>
        val sourceZpk = readKey.toArray
        val destZpk = readKey.toArray
        val maxPinLength = readNumeric(2)
        val sourcePinBlock = readHex(16).toArray
        val sourcePinBlockFormat = readString(2)
        val destinationPinBlockFormat = readString(2)
        val accountNumber = readString(12)
        TranslatePinZpkToAnotherRequest(sourceZpk, destZpk, maxPinLength, sourcePinBlock, sourcePinBlockFormat, destinationPinBlockFormat, accountNumber)
      case "DE" =>
        val pvk = readKey.toArray
        println(HexConverter.toHex(ByteString(pvk)))
        val pin = readHex(16).toArray			// TODO this is the LMK encrypted PIN, should be LN or LH
        println(HexConverter.toHex(ByteString(pin)))
        val minPinLength = readNumeric(2)
        val accountNumber = readString(12)
        val decimalisation = readStringAsBytes(16)
        val pinValidationData = readString(12)
        GenerateIBMPinOffsetRequest(pvk, pin, minPinLength, accountNumber, decimalisation, pinValidationData)
      case "EA" =>
        val zpk = readKey.toArray
        val pvk = readKey.toArray
        val maxPinLength = readNumeric(2)
        val pinBlock = readHex(16).toArray
        val pinBlockFormat = readString(2)
        val checkLength = readNumeric(2)
        val accountNumber = readString(12)
        val decimalisation = readStringAsBytes(16)
        val pinValidationData = readString(12)
        val offset = readString(12)
        VerifyInterchangePinIBMRequest(zpk, pvk, pinBlock, pinBlockFormat, checkLength, accountNumber, decimalisation, pinValidationData, offset)
      case "FA" =>
        val zmk = readKey.toArray
        val zpk = readKey.toArray
        val delOrAtalla = iter.head
        val atalla = if (delOrAtalla != ';') readNumeric(1) == 1 else false
        val delimiter = if ((delOrAtalla == ';' || atalla) && iter.hasNext) iter.getByte == ';' else false
        val (keySchemeZmk, keySchemeLmk, checkValueType) = if (delimiter) (iter.getByte, iter.getByte, iter.getByte) else ('0'.toByte, '0'.toByte, '0'.toByte)
        TranslateZpkFromZmkToLmkRequest(zmk, zpk, atalla, keySchemeZmk, keySchemeLmk, checkValueType)
      case "IA" =>
        val zmk = readKey.toArray
        val delOrAtalla = iter.head
        val atalla = if (delOrAtalla != ';') readNumeric(1) == 1 else false
        val delimiter = if ((delOrAtalla == ';' || atalla) && iter.hasNext) iter.getByte == ';' else false
        val (keySchemeZmk, keySchemeLmk, checkValueType) = if (delimiter) (iter.getByte, iter.getByte, iter.getByte) else ('0'.toByte, '0'.toByte, '0'.toByte)
        GenerateZpkRequest(zmk, atalla, keySchemeZmk, keySchemeLmk, checkValueType)
      case "GI" =>
        val encryptionIdentifier = readString(2)
        val padModeIdentifier = readString(2)
        val desKeyType = readLmkType
        val deskey = readLengthBytes
        val delimiter = iter.getByte
        val secretKeyFlag = readString(2)
        val secretKey = readLengthBytes
        val delimiter2 = iter.getByte
        val keySchemeZmk = iter.getByte
        val keySchemeLmk = iter.getByte
        ImportDesKeyRequest(deskey, secretKey, keySchemeLmk)
      case "JE" =>
        val zpk = readKey.toArray
        val pinBlock = readHex(16).toArray
        val pinBlockFormat = readString(2)
        val accountNumber = readString(12)
        TranslatePinZpkToLmkRequest(zpk, pinBlock, pinBlockFormat, accountNumber)
      case "JA" =>
        GenerateRandomPinRequest(readString(12), readString(2).toInt)
      case "MS" =>
        val messageBlockNumber = readNumeric(1)
        val keyType = readNumeric(1)
        val keyLength = readNumeric(1)
        val messageType = readNumeric(1)
        val macKey = readKey.toArray
        val iv = if (messageBlockNumber == 2 || messageBlockNumber == 3) Some(readHex(16).toArray) else None
        val messageLen = readHexNumeric(4)
        val message = if (messageType == 0) readStringAsBytes(messageLen) else readHex(messageLen * 2).toArray
        GenerateMacRequest(messageBlockNumber, keyType, keyLength, macKey, iv, message)
      case "SA" =>
        val keyType = readNumeric(1)
        val keyLength = readNumeric(4)
        val publicKeyEncoding = readNumeric(2)
        val publicExponentLength = Math.ceil(readNumeric(4) / 8.toDouble).toInt		// length is supplied in bits
        
        val publicExponent = readStringAsBytes(publicExponentLength)
        val exp = BigInt(1, publicExponent).toInt
        GenerateRSAKeySetRequest(keyType, keyLength, publicKeyEncoding, exp)
      case _ =>
        UnknownHsmRequest(cmd)
    }
  }

  val messageHeader = "    "

  def encode(msg: HsmResponse) = {
    val bs = ByteString.newBuilder

    def writeHex(b: ByteString) = bs ++= ByteString(HexConverter.toHex(b))

    def writeKey(key: ByteString) = {
      bs putByte 'U'
      writeHex(key)
    }

    def writeIntLen(len: Int, v: Int) = bs ++= ByteString(Array.fill(len)('0').mkString + f"$v%d" takeRight(len))

    def writeInt(v: Int) = bs ++= ByteString(f"$v%02d")

    def padString(s: String, len: Int, pad: Char) = s ++ Array.fill(len - s.length)(pad)
    
    bs ++= ByteString(messageHeader)
    bs ++= ByteString(msg.responseCode)
    bs ++= ByteString(msg.errorCode)
    
    msg match {
      case i: ImportDesKeyResponse =>
        writeKey(ByteString(i.desKey))
        writeHex(ByteString(i.keyCheckValue))
        bs ++= ByteString("000000000000")
      case g: GenerateRandomPinResponse =>
        bs ++= ByteString(g.pin)
      case n: NcHsmResponse =>
        bs ++= ByteString(n.lmkCheckValue)
        bs ++= ByteString(n.firmwareVersion)
      case t: TranslatePinZpkToAnotherResponse =>
        writeInt(t.pinLength)
        writeHex(ByteString(t.pinBlock))
        bs ++= ByteString(t.pinBlockFormat)
      case translateLmk: TranslatePinZpkToLmkResponse =>
        writeHex(ByteString(translateLmk.pin))		// length should be LN or LH depending on HSM configuration
      case translateZpk: TranslateZpkFromZmkToLmkResponse =>
        bs += 'U'
        writeHex(ByteString(translateZpk.zpk))
        writeHex(ByteString(translateZpk.checkValue))
        bs ++= ByteString("0000000000")
      case generateZpk: GenerateZpkResponse =>
        bs += 'X'
        writeHex(ByteString(generateZpk.zpkZmk))
        bs += 'U'
        writeHex(ByteString(generateZpk.zpkLmk))
        writeHex(ByteString(generateZpk.checkValue))
        bs ++= ByteString("0000000000")
      case generateOffset: GenerateIBMPinOffsetResponse =>
        bs ++= ByteString(padString(generateOffset.offset, 12, 'F'))
      case generateMac: GenerateMacResponse =>
        writeHex(ByteString(generateMac.mac))
      case generateRsa: GenerateRSAKeySetResponse =>
        bs ++= ByteString(generateRsa.publicKey)
        writeIntLen(4, generateRsa.privateKey.length)
        bs ++= ByteString(generateRsa.privateKey)
      case _ =>
    }

    bs.result
  }
}

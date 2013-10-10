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
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.leachbj.hsmsim.util.HexConverter
import akka.util.ByteString
import java.security.spec.X509EncodedKeySpec
import java.security.KeyFactory
import org.bouncycastle.util.encoders.Hex
import javax.crypto.Cipher
import java.io.ByteArrayInputStream
import org.bouncycastle.asn1.ASN1InputStream
import org.bouncycastle.asn1.x509.RSAPublicKeyStructure
import java.security.spec.RSAPublicKeySpec

@RunWith(classOf[JUnitRunner])
class GenerateRSAKeySetSuite extends FunSuite {
  test("should generate random key") {
    val req = GenerateRSAKeySetRequest(0, 512, 1, 0x1001)
    val resp = GenerateRSAKeySetResponse.createResponse(req);
    assert(resp.errorCode === "00")
    assert(resp.responseCode === "SB")
    assert(resp.isInstanceOf[GenerateRSAKeySetResponse])
    val generateResponse = resp.asInstanceOf[GenerateRSAKeySetResponse]
    println(HexConverter.toHex(ByteString(generateResponse.publicKey)))
    println(HexConverter.toHex(ByteString(generateResponse.privateKey)))

    checkKey(generateResponse.publicKey, generateResponse.privateKey)
  }

  def checkKey(publicKey: Array[Byte], privateKey: Array[Byte]) = {
    val des = encryptKeyUnderRsa(publicKey, HexConverter.fromHex("7AF4D50EE6587A767AF4D50EE6587A76").toArray)
    val desImportReq = ImportDesKeyRequest(des, privateKey, 'T')
    val importDesKeyResponse = ImportDesKeyResponse.createResponse(desImportReq).asInstanceOf[ImportDesKeyResponse]
    assert(importDesKeyResponse.desKey === HexConverter.fromHex("0157015564C146EB90920C60CAB2E8F6").toArray)
    assert(importDesKeyResponse.keyCheckValue === HexConverter.fromHex("FD7EC34F674D").toArray)
  }

  private def encryptKeyUnderRsa(key: Array[Byte], data: Array[Byte]) = {
    val keyFactory = KeyFactory.getInstance("RSA");
    val rsaKey = RSAPublicKeyStructure.getInstance(toDERObject(key))
    val keySpec = new RSAPublicKeySpec(rsaKey.getModulus(), rsaKey.getPublicExponent());
    val publicKey = keyFactory.generatePublic(keySpec);
    val cipher = Cipher.getInstance("RSA/ECB/PKCS1Padding");

    cipher.init(Cipher.ENCRYPT_MODE, publicKey);
    cipher.doFinal(data);
  }

  private def toDERObject(data: Array[Byte]) = {
    def inStream = new ByteArrayInputStream(data);
    def asnInputStream = new ASN1InputStream(inStream);

    asnInputStream.readObject();
  }
  //  BitLength: 512
  //00F308DB9DECE7437E86C90E5744CED460BC9FE083C5A6F224B9ECB1B15D32475100D1DB72C4E2B31377327BA006D04711DDD3F7D72F5CF34168BD8F9495BE0170EB00C951BD1AC4F38D6572217A4FB01FE4AD7D92B2587592256366E03B11C131B501009E1E0D8C610EF254B639C56A3CD5BF9577175F0FF5A5E71BA6F01ED0F0779F7F0072C58FF99338EBC7A44E3E86465211FA52F6703586AFE50F2CDEC72DB321407C000000
  //305C300D06092A864886F70D0101010500034B003048024100C73A90D4AA4EC3E7324E2DF3CA7793C2BA6F60681A43E40419ACEE019166E1DA79D2410A5981A924962D4813DBC05147A68912035E1D6C9D819EBADBF1ABE75B0203010001
  //45BFE93B6D92739ADF44679ACAF9292D65330CFEA1C1C17BC037C689679D6BDA52FE75098FB8957DE82D7401FB0234FDD49310AD61DCD9A6D750D1991115BCED7611A5A0CAE8721BC356442392E88521175F25176D293E77FB4E524DF99ADA06133FE2250C66308E54850B3F92FD901028ABB462ED9CCE0C4133C3F7A2066F92BE8D6B9CDD7C49AF2B3E190D578B051BE688BBF5B417A63111DE26B390D775E8C13A74CC2E2D8CE1348D054067120C98

}
// The AWS Client Side Side V4 Signature Process

import com.amazonaws.auth.SigningAlgorithm
import com.amazonaws.util.BinaryUtils

val signer = new com.amazonaws.auth.AWS4Signer()

// The AWS Secret Key suppied to Users
val secret_key = "271a52a20e7c4e14b67ee93894f0d0e0"

// Expected Signature this algo should produce
val expected = "2301e2a7105206bb052e13b7ed1adaf92633d2ca04174c0655f59aab8ee9dd34"

// This is made by hashing a bunch of request info stuff (method, path, headers, body)
// https://github.com/aws/aws-sdk-java/blob/master/src/main/java/com/amazonaws/auth/AWS4Signer.java#L244-L258
// https://github.com/aws/aws-sdk-java/blob/master/src/main/java/com/amazonaws/auth/AWS4Signer.java#L265
val canonicalRequestHash = "274c7a3e25a34a5724d5ad86ebab11b7655b6df3d7e675f6f663af0a536ecb55"

val dateStamp = "20140606"
val timeStamp = "20140606T025023Z"

val regionName = "us-east-1"
val serviceName = "dynamodb"
val TERMINATOR = "aws4_request"

val stringToSign = "AWS4-HMAC-SHA256\n" + timeStamp + "\n" + datestamp + "/" +
                    regionName + "/" + serviceName + "/" + TERMINATOR + "\n" +
                    canonicalRequestHash

val kSecret = ("AWS4" + secret_key).getBytes()
val kDate = signer.sign(dateStamp, kSecret, SigningAlgorithm.HmacSHA256)
val kRegion = signer.sign(regionName, kDate, SigningAlgorithm.HmacSHA256)
val kService = signer.sign(serviceName, kRegion, SigningAlgorithm.HmacSHA256)
val kSigning = signer.sign(TERMINATOR, kService, SigningAlgorithm.HmacSHA256)

val signature = signer.sign(stringToSign, kSigning, SigningAlgorithm.HmacSHA256)

val result = BinaryUtils.toHex(signature)

expected == result

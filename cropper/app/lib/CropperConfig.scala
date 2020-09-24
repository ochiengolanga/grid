package lib

import java.io.File

import com.amazonaws.auth.{AWSCredentials, BasicAWSCredentials}
import com.gu.mediaservice.lib.config.CommonConfig
import play.api.Configuration


class CropperConfig(override val playAppConfiguration: Configuration) extends CommonConfig {

  final override lazy val appName = "cropper"

  val imgPublishingBucket = string("publishing.image.bucket")

  val imgPublishingHost = string("publishing.image.host")
  // Note: work around CloudFormation not allowing optional parameters
  val imgPublishingSecureHost = stringOpt("publishing.image.secure.host").filterNot(_.isEmpty)

  val rootUri = services.cropperBaseUri
  val apiUri = services.apiBaseUri
  val kahunaUri = services.kahunaBaseUri
  val loginUriTemplate = services.loginUriTemplate

  val tempDir: File = new File(stringDefault("crop.output.tmp.dir", "/tmp"))

  val landscapeCropSizingWidths = List(2000, 1000, 500, 140)
  val portraitCropSizingHeights = List(2000, 1000, 500)
}

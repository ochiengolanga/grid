package lib

import java.io.File

import com.gu.mediaservice.lib.cleanup.{ImageProcessor, ImageProcessorConfig, MetadataCleaners, SupplierProcessors}
import com.gu.mediaservice.lib.config.{CommonConfig, MetadataConfig}
import com.gu.mediaservice.model._
import com.typesafe.scalalogging.StrictLogging
import play.api.Configuration

import scala.util.matching.Regex

class ImageLoaderConfig(override val configuration: Configuration) extends CommonConfig with StrictLogging {

  final override lazy val appName = "image-loader"

  val imageBucket: String = properties("s3.image.bucket")

  val thumbnailBucket: String = properties("s3.thumb.bucket")

  val tempDir: File = new File(properties.getOrElse("upload.tmp.dir", "/tmp"))

  val thumbWidth: Int = 256
  val thumbQuality: Double = 85d // out of 100

  val rootUri: String = services.loaderBaseUri
  val apiUri: String = services.apiBaseUri
  val loginUriTemplate: String = services.loginUriTemplate

  val transcodedMimeTypes: List[MimeType] = getStringSetFromProperties("transcoded.mime.types").toList.map(MimeType(_))
  val supportedMimeTypes: List[MimeType] = List(Jpeg, Png) ::: transcodedMimeTypes

  val imageProcessors: List[ImageProcessor] = {
    val ImageProcessorClass: Regex = "class:(.*)".r

    val imageProcessorMaybes: List[Either[String, ImageProcessor]] = getStringListFromProperties("image.processors", "SupplierProcessors, MetadataCleaners").map {
      case ImageProcessorClass(className) => ImageProcessorClassReflector.loadImageProcessor(className, ImageProcessorConfig())
      case "SupplierProcessors" => scala.Right(SupplierProcessors)
      case "MetadataCleaners" => scala.Right(new MetadataCleaners(MetadataConfig.allPhotographersMap))
      case other => Left(s"Unable to parse image processor ${other}")
    }
    val imageProcessorErrors: List[String] = imageProcessorMaybes.collect{
      case Left(error) => error
    }
    if (imageProcessorErrors.nonEmpty) {
      imageProcessorErrors.foreach(err => logger.error(err))
      throw new IllegalArgumentException("Unable to load all configured image.processors, see logs for details")
    }

    imageProcessorMaybes.collect{
      case scala.Right(processor) => processor
    }
  }
}

package lib

import com.gu.mediaservice.lib.cleanup.{ImageProcessor, ImageProcessorConfig}
import com.typesafe.scalalogging.StrictLogging

import scala.language.existentials
import scala.util.Try
import scala.util.control.NonFatal

object ImageProcessorClassReflector extends StrictLogging {
  def loadImageProcessor(className: String, config: ImageProcessorConfig): Either[String, ImageProcessor] = {
    for {
      imageProcessorClass <- loadClass(className)
      imageProcessorInstance <- instantiate(imageProcessorClass, config)
    } yield imageProcessorInstance
  }

  private def loadClass(className: String): Either[String, Class[_]] = catchNonFatal(Class.forName(className)){
    case _:ClassNotFoundException => s"Unable to find image processor class $className"
    case other =>
      logger.error(s"Error whilst loading $className", other)
      s"Unknown error whilst loading $className, check logs"
  }

  private def instantiate(imageProcessorClass: Class[_], config: ImageProcessorConfig): Either[String, ImageProcessor] = {
    val maybeNoArgCtor = Try(imageProcessorClass.getDeclaredConstructor()).toOption
    val maybeConfigCtor = Try(imageProcessorClass.getDeclaredConstructor(classOf[ImageProcessorConfig])).toOption
    for {
      instance <- (maybeNoArgCtor, maybeConfigCtor) match {
        case (_, Some (configCtor)) => Right (configCtor.newInstance (config))
        case (Some (noArgCtor), None) => Right (noArgCtor.newInstance ())
        case (None, None) => Left (s"Unable to find a suitable constructor for ${imageProcessorClass.getCanonicalName}. Must either have a no arg constructor or a constructor taking one argument of type ImageProcessorConfig.")
      }
      castInstance <- try {
        Right(instance.asInstanceOf[ImageProcessor])
      } catch {
        case _:ClassCastException => Left(s"Failed to cast ${imageProcessorClass.getCanonicalName} to an ImageProcessor")
      }
    } yield castInstance
  }

  private def catchNonFatal[T](block: => T)(error: Throwable => String): Either[String, T] = {
    try {
      Right(block)
    } catch {
      case NonFatal(e) => Left(error(e))
    }
  }
}

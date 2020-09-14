package lib

import com.gu.mediaservice.lib.cleanup.{ImageProcessor, ImageProcessorConfig}
import com.gu.mediaservice.lib.config.CommonConfig
import com.typesafe.scalalogging.StrictLogging

import scala.reflect.runtime.universe
import scala.util.control.NonFatal

object ImageProcessorClassReflector extends StrictLogging {
  def catchNonFatal[T](block: => T): Either[Throwable, T] = {
    try {
      Right(block)
    } catch {
      case NonFatal(e) => Left(e)
    }
  }

  def loadImageProcessor(className: String, config: ImageProcessorConfig): Either[String, ImageProcessor] = {
    val mirror = universe.runtimeMirror(getClass.getClassLoader)

    try {
      for {
        classSymbol <- catchNonFatal(mirror.staticClass(className)).left.map(_ => s"Class $className not found")
        isImageProcessor = classSymbol.baseClasses.exists(symbol => symbol.fullName == classOf[ImageProcessor].getCanonicalName)
        _ <- if (isImageProcessor) Right(()) else Left(s"Configured image processor $className does not extend ${classOf[ImageProcessor].getCanonicalName}")
        classMirror = mirror.reflectClass(classSymbol)
        constructor = classSymbol.primaryConstructor.asMethod
        params = constructor.paramLists.head
        hasConfigParam = {
          val configParamType = Class.forName(params.head.info.toString, true, getClass.getClassLoader)
          val assignable = classOf[ImageProcessorConfig].isAssignableFrom(configParamType)
          System.err.println(s"${params.length} ${classOf[CommonConfig].getCanonicalName} ${configParamType} ${assignable}")
          params.length == 1 && assignable
        }
        hasUnexpectedParams = params.nonEmpty && !hasConfigParam
        _ <- if (hasUnexpectedParams) Left(s"Cannot instantiate constructor as has unexpected parameters") else Right(())
        methodMirror = classMirror.reflectConstructor(constructor)
        instance = if (hasConfigParam) methodMirror(config) else methodMirror()
      } yield instance.asInstanceOf[ImageProcessor]
    } catch {
      case NonFatal(e) =>
        logger.error(s"Unable to load image processor $className due to unknown exception", e)
        Left(s"Unknown exception whilst loading $className")
    }
  }
}

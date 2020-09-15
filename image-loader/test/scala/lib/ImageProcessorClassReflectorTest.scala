package scala.lib

import com.gu.mediaservice.lib.cleanup.{ImageProcessor, ImageProcessorConfig}
import com.gu.mediaservice.model.{Image, UploadInfo}
import lib.ImageProcessorClassReflector
import org.joda.time.DateTime
import org.scalatest.{EitherValues, FreeSpec, Matchers}

class NoArgImageProcessor extends ImageProcessor {
  override def apply(image: Image): Image = image.copy(id = "no-arg-image-processed")
}

class ConfigImageProcessor(config: ImageProcessorConfig) extends ImageProcessor {
  override def apply(image: Image): Image = image.copy(id = s"config-image-processed ${config.hashCode}")
}

class NotAnImageProcessor {
  def apply(image: Image): Image = image.copy(id = "not-image-processed")
}

class ImageProcessorWithStringConstructor(configString: String) extends ImageProcessor {
  def apply(image: Image): Image = image.copy(id = "not-image-processed")
}

class ImageProcessorClassReflectorTest extends FreeSpec with Matchers with EitherValues {
  val testImage: Image = Image("image", DateTime.now(), "Test", None, Map.empty, null, null, null, null, null, null, null, null, null, null)
  val testConfig: ImageProcessorConfig = ImageProcessorConfig()

  "The class reflector" - {
    "should successfully load a no arg ImageProcessor instance" in {
      val instance = ImageProcessorClassReflector.loadImageProcessor(classOf[NoArgImageProcessor].getCanonicalName, testConfig)
      instance.right.value.apply(testImage).id shouldBe "no-arg-image-processed"
    }

    "should successfully load a config arg ImageProcessor instance" in {
      val instance = ImageProcessorClassReflector.loadImageProcessor(classOf[ConfigImageProcessor].getCanonicalName, testConfig)
      instance.right.value.apply(testImage).id shouldBe s"config-image-processed ${testConfig.hashCode}"
    }

    "should fail to load something that isn't an ImageProcessor" in {
      val instance = ImageProcessorClassReflector.loadImageProcessor(classOf[NotAnImageProcessor].getCanonicalName, testConfig)
      instance.left.value shouldBe "Failed to cast scala.lib.NotAnImageProcessor to an ImageProcessor"
    }

    "should fail to load something that doesn't have a suitable constructor" in {
      val instance = ImageProcessorClassReflector.loadImageProcessor(classOf[ImageProcessorWithStringConstructor].getCanonicalName, testConfig)
      instance.left.value shouldBe "Unable to find a suitable constructor for scala.lib.ImageProcessorWithStringConstructor. Must either have a no arg constructor or a constructor taking one argument of type ImageProcessorConfig."
    }

    "should fail to load something that doesn't exist" in {
      val instance = ImageProcessorClassReflector.loadImageProcessor("scala.lib.ImageProcessorThatDoesntExist", testConfig)
      instance.left.value shouldBe "Unable to find image processor class scala.lib.ImageProcessorThatDoesntExist"
    }
  }
}

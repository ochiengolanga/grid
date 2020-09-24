import com.gu.mediaservice.lib.imaging.ImageOperations
import com.gu.mediaservice.lib.management.ManagementWithPermissions
import com.gu.mediaservice.lib.play.GridComponents
import controllers.CropperController
import lib.{CropStore, CropperConfig, Crops, Notifications}
import play.api.ApplicationLoader.Context
import router.Routes

class CropperComponents(context: Context) extends GridComponents(context) {
  final override lazy val config = new CropperConfig(configuration, context.environment.mode)

  final override val buildInfo = utils.buildinfo.BuildInfo

  val store = new CropStore(config)
  val imageOperations = new ImageOperations(context.environment.rootPath.getAbsolutePath)

  val crops = new Crops(config, store, imageOperations)
  val notifications = new Notifications(config)

  val controller = new CropperController(auth, crops, store, notifications, config, controllerComponents, wsClient)
  val permissionsAwareManagement = new ManagementWithPermissions(controllerComponents, controller, buildInfo)

  override lazy val router = new Routes(httpErrorHandler, controller, permissionsAwareManagement)
}

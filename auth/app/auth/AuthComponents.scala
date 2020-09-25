package auth

import com.gu.mediaservice.lib.management.ManagementWithPermissions
import com.gu.mediaservice.lib.play.GridComponents
import play.api.ApplicationLoader.Context
import play.api.{Configuration, Environment}
import play.api.http.HttpConfiguration
import router.Routes

class AuthComponents(context: Context) extends GridComponents(context) {
  final override lazy val config = new AuthConfig(configuration)
  final override lazy val httpConfiguration = AuthHttpConfig(configuration, context.environment)

  final override val buildInfo = utils.buildinfo.BuildInfo

  val controller = new AuthController(auth, config, controllerComponents)
  val permissionsAwareManagement = new ManagementWithPermissions(controllerComponents, controller, buildInfo)

  override val router = new Routes(httpErrorHandler, controller, permissionsAwareManagement)
}

object AuthHttpConfig {
  def apply(playConfig: Configuration, environment: Environment): HttpConfiguration = {
    val base = HttpConfiguration.fromConfiguration(playConfig, environment)
    base.copy(session =
      base.session.copy(sameSite = None)
    )
  }
}

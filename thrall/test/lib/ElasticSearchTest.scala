package lib

import com.gu.mediaservice.lib.elasticsearch.ElasticSearchConfig
import com.whisk.docker.{DockerContainer, DockerReadyChecker}
import play.api.Configuration

import scala.concurrent.duration._

class ElasticSearchTest extends ElasticSearchTestBase {

  val elasticSearchConfig = ElasticSearchConfig("writealias", es6TestUrl, "media-service-test", 1, 0)

  val ES = new ElasticSearch(elasticSearchConfig, None)
  val esContainer = if (useEsDocker) Some(DockerContainer("docker.elastic.co/elasticsearch/elasticsearch:7.5.2")
    .withPorts(9200 -> Some(9200))
    .withEnv("cluster.name=media-service", "xpack.security.enabled=false", "discovery.type=single-node", "network.host=0.0.0.0")
    .withReadyChecker(
      DockerReadyChecker.HttpResponseCode(9200, "/", Some("0.0.0.0")).within(10.minutes).looped(40, 1250.millis)
    )
  ) else None
}
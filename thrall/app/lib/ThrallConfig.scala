package lib

import com.amazonaws.auth.AWSCredentialsProvider
import com.gu.mediaservice.lib.config.CommonConfig
import org.joda.time.DateTime
import org.joda.time.format.ISODateTimeFormat
import play.api.Configuration

case class ThrallKinesisConfig(
  thrallKinesisStream: String,
  thrallKinesisEndpoint: String,
  thrallKinesisDynamoEndpoint: String,
  awsRegion: String,
  rewindFrom: Option[DateTime],
  awsCredentials: AWSCredentialsProvider
)

class ThrallConfig(override val configuration: Configuration) extends CommonConfig {
  final override lazy val appName = "thrall"

  lazy val queueUrl: String = properties("sqs.queue.url")

  lazy val imageBucket: String = properties("s3.image.bucket")

  lazy val writeAlias: String = properties.getOrElse("es.index.aliases.write", configuration.get[String]("es.index.aliases.write"))

  lazy val thumbnailBucket: String = properties("s3.thumb.bucket")

  lazy val elasticsearch6Url: String =  properties("es6.url")
  lazy val elasticsearch6Cluster: String = properties("es6.cluster")
  lazy val elasticsearch6Shards: Int = properties("es6.shards").toInt
  lazy val elasticsearch6Replicas: Int = properties("es6.replicas").toInt

  lazy val metadataTopicArn: String = properties("indexed.image.sns.topic.arn")

  lazy val rewindFrom: Option[DateTime] = properties.get("thrall.kinesis.stream.rewindFrom").map(ISODateTimeFormat.dateTime.parseDateTime)
  lazy val lowPriorityRewindFrom: Option[DateTime] = properties.get("thrall.kinesis.lowPriorityStream.rewindFrom").map(ISODateTimeFormat.dateTime.parseDateTime)

  def kinesisConfig: ThrallKinesisConfig = ThrallKinesisConfig(thrallKinesisStream, thrallKinesisEndpoint,thrallKinesisDynamoEndpoint, awsRegion, rewindFrom, awsCredentials)
  def kinesisLowPriorityConfig: ThrallKinesisConfig = ThrallKinesisConfig(thrallKinesisLowPriorityStream, thrallKinesisEndpoint,thrallKinesisDynamoEndpoint, awsRegion, lowPriorityRewindFrom, awsCredentials)

}

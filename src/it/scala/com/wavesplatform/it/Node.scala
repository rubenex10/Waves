package com.wavesplatform.it

import java.io.IOException

import com.typesafe.config.Config
import com.wavesplatform.settings.WavesSettings
import io.netty.util.{HashedWheelTimer, Timeout, Timer}
import org.asynchttpclient.Dsl.{get => _get, post => _post}
import org.asynchttpclient._
import org.asynchttpclient.util.HttpConstants
import org.slf4j.LoggerFactory
import play.api.libs.json.Json._
import play.api.libs.json._
import scorex.api.http.alias.CreateAliasRequest
import scorex.api.http.assets.TransferRequest
import scorex.utils.{LoggerFacade, ScorexLogging}

import scala.compat.java8.FutureConverters._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}


class Node(config: Config, nodeInfo: NodeInfo, client: AsyncHttpClient, timer: HashedWheelTimer) {
  import Node._

  val privateKey = config.getString("private-key")
  val publicKey = config.getString("public-key")
  val address = config.getString("address")
  val settings = WavesSettings.fromConfig(config)

  private val generationDelay = settings.minerSettings.generationDelay
  private val log = LoggerFacade(LoggerFactory.getLogger(s"${getClass.getName}.${settings.networkSettings.nodeName}"))

  private def retrying(r: Request, interval: FiniteDuration = 200.millis): Future[Response] =
    timer.retryUntil[Response](client.executeRequest(r).toCompletableFuture.toScala, _ => true, interval)

  def get(path: String, f: RequestBuilder => RequestBuilder = identity): Future[Response] =
    retrying(f(_get(s"http://localhost:${nodeInfo.hostRestApiPort}$path")).build())

  def post(path: String, f: RequestBuilder => RequestBuilder = identity): Future[Response] =
    retrying(f(
      _post(s"http://localhost:${nodeInfo.hostRestApiPort}$path").setHeader("api_key", "integration-test-rest-api")
    ).build())

  def post[A: Writes](path: String, body: A): Future[Response] =
    post(path, (rb: RequestBuilder) => rb.setHeader("Content-type", "application/json").setBody(stringify(toJson(body))))

  def connectedPeers: Future[Seq[Peer]] = get("/peers/connected").map { r =>
    (Json.parse(r.getResponseBody) \ "peers").as[Seq[Peer]]
  }

  def height: Future[Long] = get("/blocks/height").as[JsValue].map(v => (v \ "height").as[Long])
  def blockAt(height: Long) = get(s"/blocks/at/$height").as[Block]
  def lastBlock: Future[Block] = get("/blocks/last").as[Block]
  def blockSeq(from: Long, to: Long) = get(s"/blocks/seq/$from/$to").as[Seq[Block]]
  def status: Future[Status] = get("/node/status").as[Status]
  def balance(address: String): Future[Balance] = get(s"/addresses/balance/$address").as[Balance]
  def transfer(sourceAddress: String, recipient: String, amount: Long, fee: Long): Future[Transaction] =
    post("/assets/transfer", TransferRequest(None, None, amount, fee, sourceAddress, None, recipient)).as[Transaction]

  def createAlias(targetAddress: String, alias: String, fee: Long): Future[Transaction] =
    post("/alias/create", CreateAliasRequest(targetAddress, alias, fee)).as[Transaction]

  def waitForHeight(targetHeight: Long): Future[Long] =
    timer.retryUntil[Long](height, _ >= targetHeight, generationDelay)
  def waitForNextBlock: Future[Block] = for {
    currentBlock <- lastBlock
    actualBlock <- findBlock(_.height > currentBlock.height, currentBlock.height)
  } yield actualBlock

  def findBlock(cond: Block => Boolean, from: Long = 1, to: Long = Long.MaxValue): Future[Block] = {
    val p = Promise[Block]

    def reschedule(_from: Long, _to: Long) = try {
      timer.newTimeout(retrying(_from, _to), (generationDelay.toMillis * 1.5).toLong, MILLISECONDS)
    } catch {
      case NonFatal(t) => p.failure(t)
    }

    def retrying(_from: Long, _to: Long)(timeout: Timeout): Unit = blockSeq(_from, _to).onComplete {
      case Success(blocks) =>
        blocks.find(cond) match {
          case Some(b) =>
            log.debug(s"Found matching block ${b.signature}")
            p.success(b)
          case None =>
            val newFrom = blocks.lastOption.fold(_from)(_.height + 1)
            if (newFrom > to) {
              p.failure(new NoSuchElementException)
            } else {
              log.debug(s"Loaded ${blocks.length} blocks, no match found. Next range: [$newFrom, ${newFrom + 99}]")
              reschedule(newFrom, (newFrom + 99).min(to))
            }
        }
      case Failure(t) =>
        log.debug("Error loading blocks", t)
        reschedule(_from, _to)
    }

    retrying(from, from + 99)(null)

    p.future
  }
}

object Node extends ScorexLogging {
  case class Status(blockGeneratorStatus: String, historySynchronizationStatus: String)
  implicit val statusFormat: Format[Status] = Json.format

  case class Peer(address: String, declaredAddress: String, peerName: String)
  implicit val peerFormat: Format[Peer] = Json.format

  case class Balance(address: String, confirmations: Int, balance: Long)
  implicit val balanceFormat: Format[Balance] = Json.format

  case class Transaction(`type`: Int, id: String, fee: Long, timestamp: Long)
  implicit val transactionFormat: Format[Transaction] = Json.format

  case class Block(signature: String, height: Long, timestamp: Long, generator: String, transactions: Seq[Transaction])
  implicit val blockFormat: Format[Block] = Json.format

  implicit class ResponseFutureExt(val f: Future[Response]) extends AnyVal {
    def as[A: Format](implicit ec: ExecutionContext): Future[A] =
      f.transform {
        case Success(r) if r.getStatusCode == HttpConstants.ResponseStatusCodes.OK_200 =>
          Try(parse(r.getResponseBody).as[A])
        case Success(r) =>
          log.debug(s"Error parsing response ${r.getResponseBody}")
          Failure(new IOException(s"Unexpected status code: ${r.getStatusCode}"))
        case Failure(t) => Failure[A](t)
      }(ec)
  }

  implicit class TimerExt(val timer: Timer) extends AnyVal {
    def retryUntil[A](future: => Future[A], cond: A => Boolean, interval: FiniteDuration): Future[A] = {
      val p = Promise[A]

      def retrying(timeout: Timeout): Unit = future.onComplete {
        case Success(v) if cond(v) => p.success(v)
        case _ => try {
          timer.newTimeout(retrying, interval.toMillis, MILLISECONDS)
        } catch {
          case NonFatal(e) => p.failure(e)
        }
      }

      retrying(null)

      p.future
    }
  }
}
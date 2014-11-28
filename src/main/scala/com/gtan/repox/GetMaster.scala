package com.gtan.repox

import java.net.URL
import java.nio.file.{Files, Path, StandardCopyOption}

import akka.actor.SupervisorStrategy.{Escalate, Resume}
import akka.actor._
import com.gtan.repox.GetWorker.{WorkerDead, Cleanup, PeerChosen}
import com.gtan.repox.Head404Cache.Query
import com.gtan.repox.SourceCache.{WhichRepo, Clear, Source}
import com.ning.http.client.FluentCaseInsensitiveStringsMap
import com.typesafe.scalalogging.LazyLogging
import io.undertow.Handlers
import io.undertow.server.HttpServerExchange
import io.undertow.server.handlers.resource.FileResourceManager

import scala.collection.JavaConverters._
import scala.language.postfixOps
import scala.util.Random

/**
 * Created by IntelliJ IDEA.
 * User: xf
 * Date: 14/11/21
 * Time: 下午10:01
 */

import akka.pattern.ask

object GetMaster extends LazyLogging {

  import concurrent.duration._
  import scala.concurrent.ExecutionContext.Implicits.global

  implicit val timeout = new akka.util.Timeout(1 seconds)
}

class GetMaster(exchange: HttpServerExchange,
                resolvedPath: Path) extends Actor with ActorLogging {

  import scala.concurrent.duration._

  val uri = exchange.getRequestURI
  val requestHeaders = new FluentCaseInsensitiveStringsMap()
  for (name <- exchange.getRequestHeaders.getHeaderNames.asScala) {
    requestHeaders.add(name.toString, exchange.getRequestHeaders.get(name))
  }

  override val supervisorStrategy =
    OneForOneStrategy(maxNrOfRetries = 3, withinTimeRange = 1 minute)(super.supervisorStrategy.decider)


  private def reduce(xss: List[List[Repo]], notFoundIn: Set[Repo]): List[List[Repo]] = {
    xss.map(_.filterNot(notFoundIn.contains)).filter(_.nonEmpty)
  }

  var getterChosen = false
  var chosen: ActorRef = null
  var child404Count = 0
  var candidateRepos = Repox.candidates
  var thisLevel: List[Repo] = _
  var children: List[ActorRef] = _

  def waitFor404Cache: Receive = {
    case Head404Cache.ExcludeRepos(repos) =>
      candidateRepos = reduce(candidateRepos, repos)
      log.debug(s"Try ${candidateRepos.map(_.map(_.name))} $uri")
      thisLevel = candidateRepos.head
      children = for (upstream <- thisLevel) yield {
        val upstreamUrl = upstream.base + uri
        val upstreamHost = new URL(upstreamUrl).getHost
        requestHeaders.put("Host", List(upstreamHost).asJava)
        requestHeaders.put("Accept-Encoding", List("identity").asJava)
        val childActorName = upstream.name
        context.actorOf(
          Props(classOf[GetWorker], upstream, uri, requestHeaders),
          name = s"GetWorker_$childActorName"
        )
      }
      context become working
  }

  def askHead404Cache(): Unit = {
    Repox.head404Cache ! Query(exchange.getRequestURI)
  }

  askHead404Cache()
  def receive = waitFor404Cache

  def working: Receive = {
    case GetWorker.UnsuccessResponseStatus(status) =>
      child404Count += 1
      if (child404Count == children.length) {
        candidateRepos match {
          case Nil =>
            log.info(s"GetMaster all child failed. 404")
            context.parent ! GetQueueWorker.Get404(uri)
            self ! PoisonPill
          case head :: tail =>
            log.info(s"all child failed. to next level.")
            candidateRepos = tail
            child404Count = 0
            askHead404Cache()
            context become waitFor404Cache
        }
      }
    case msg @ GetWorker.Completed(path, repo) =>
      if (sender == chosen) {
        resolvedPath.getParent.toFile.mkdirs()
        Files.move(path, resolvedPath, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.ATOMIC_MOVE)
        log.info(s"GetWorker ${sender().path.name} completed. GetMaster served GET request ${exchange.getRequestURI}.")
        context.parent ! GetQueueWorker.Completed(path, repo, exchange)
        children.foreach(child => child ! Cleanup)
        if (!path.endsWith(".sha1")) {
          Repox.sourceCache ! Source(exchange.getRequestURI, repo)
        } else {
          Repox.sourceCache ! Clear(exchange.getRequestURI)
        }
        self ! PoisonPill
      } else {
        sender ! Cleanup
      }
    case GetWorker.HeadersGot(_) =>
      if (!getterChosen) {
        log.debug(s"chose ${sender().path.name}, canceling others.")
        for (others <- children.filterNot(_ == sender())) {
          others ! PeerChosen(sender())
        }
        chosen = sender()
        getterChosen = true
      } else if (sender != chosen) {
        sender ! PeerChosen(chosen)
      }
    case WorkerDead =>
      if (sender != chosen)
        sender ! Cleanup
      else throw new Exception("Chosen worker dead. Restart and rechoose.")
  }


}

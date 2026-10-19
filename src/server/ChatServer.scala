package server

import org.apache.pekko.actor.typed.ActorSystem
import org.apache.pekko.actor.typed.scaladsl.Behaviors
import org.apache.pekko.http.scaladsl.Http
import org.apache.pekko.http.scaladsl.model._
import org.apache.pekko.http.scaladsl.model.headers._
import org.apache.pekko.http.scaladsl.server.Directives._
import org.apache.pekko.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import spray.json._
import spray.json.DefaultJsonProtocol._

import models.Models._
import modules.CoreChatbot._
import modules.RecommendationEngine._
import modules.ConversationMemory._

import scala.concurrent.{Await, ExecutionContext}

import scala.collection.mutable

object ChatServer {

  // ─────────────────────────────────────────────
  // Step 1: JSON contract
  // ─────────────────────────────────────────────
  case class ChatRequest(sessionId: String, message: String)
  case class ChatResponse(response: String)

  // ─────────────────────────────────────────────
  // Step 2: spray-json formats
  // .apply is required explicitly in Scala 3
  // ─────────────────────────────────────────────
  implicit val chatRequestFormat:  RootJsonFormat[ChatRequest]  = jsonFormat2(ChatRequest.apply)
  implicit val chatResponseFormat: RootJsonFormat[ChatResponse] = jsonFormat1(ChatResponse.apply)

  // ─────────────────────────────────────────────
  // Step 3: Session store
  // ─────────────────────────────────────────────
  val sessions: mutable.Map[String, ConversationState] = mutable.Map.empty

  // ─────────────────────────────────────────────
  // Step 4: Core handler
  // ─────────────────────────────────────────────
  def processMessage(sessionId: String, userInput: String): String = {

    val state = sessions.getOrElse(sessionId, ConversationState.empty)

    val (response, intent) = handleUserInput(userInput, state)

    val (finalResponse, newState) = intent match {

      case ExitRequest =>
        val r = response
        (r, logInteraction(userInput, r, state))

      case SummaryRequest =>
        val r = handleSummaryRequest("[SUMMARY_REQUEST]", state)
        (r, logInteraction(userInput, r, state))

      case AnalysisRequest =>
        val r = handleSummaryRequest("[ANALYSIS_REQUEST]", state)
        (r, logInteraction(userInput, r, state))

      case _ =>
        val resolved = response match {
          case r if r.contains("[RECOMMENDATION_REQUEST:") =>
            val tag = extractTag(r, "[RECOMMENDATION_REQUEST:")
            handleRecommendationRequest(tag, state)

          case r if r.contains("[EXPLANATION_REQUEST:") =>
            val tag = extractTag(r, "[EXPLANATION_REQUEST:")
            handleRecommendationRequest(tag, state)

          case r if r.contains("[PREFERENCE_UPDATE:") =>
            val tag = extractTag(r, "[PREFERENCE_UPDATE:")
            val parts = tag.replace("[PREFERENCE_UPDATE:", "").replace("]", "").split(":")
            if (parts.length == 2)
              sessions.update(sessionId, updatePreferences(parts(0), parts(1), state))
            handleRecommendationRequest(tag, state)

          case r => r
        }

        val repeatNote =
          if (detectRepeatedQuery(userInput, state.history))
            "\n(Note: You have asked about this before. Here is my response again.)\n"
          else ""

        val moodPrefix = getUserMood(state.history) match {
          case "negative" => "I sense some frustration. Let me try to help better.\n"
          case "positive" => "Great energy! Let us keep going.\n"
          case _          => ""
        }

        val full = moodPrefix + repeatNote + resolved
        (full, logInteraction(userInput, full, state))
    }

    sessions.update(sessionId, newState)
    finalResponse
  }

  // ─────────────────────────────────────────────
  // Step 5: Tag extractor
  // ─────────────────────────────────────────────
  def extractTag(response: String, tagStart: String): String = {
    val start = response.indexOf(tagStart)
    val end   = response.indexOf("]", start)
    if (start >= 0 && end >= 0) response.substring(start, end + 1)
    else ""
  }

  // ─────────────────────────────────────────────
  // Step 6: CORS header
  // ─────────────────────────────────────────────
  val corsHeader = `Access-Control-Allow-Origin`.*

  // ─────────────────────────────────────────────
  // Step 7: Route
  // concat() replaces ~ to avoid Scala 3 line
  // parsing issues with the tilde operator
  // ─────────────────────────────────────────────
  def route =
    respondWithHeader(corsHeader) {
      path("chat") {
        concat(
          options {
            respondWithHeaders(
              RawHeader("Access-Control-Allow-Methods", "POST, OPTIONS"),
              RawHeader("Access-Control-Allow-Headers", "Content-Type")
            ) {
              complete(HttpResponse(StatusCodes.OK))
            }
          },
          post {
            entity(as[ChatRequest]) { req =>
              val reply = processMessage(req.sessionId, req.message)
              complete(ChatResponse(reply))
            }
          }
        )
      }
    }

  // ─────────────────────────────────────────────
  // Step 8: Entry point
  // ─────────────────────────────────────────────
  def main(args: Array[String]): Unit = {

    implicit val system: ActorSystem[Nothing] =
      ActorSystem(Behaviors.empty, "probot-system")

    implicit val ec: ExecutionContext = system.executionContext

    Http().newServerAt("localhost", 8080).bind(route)

    println("╔══════════════════════════════════════════╗")
    println("║  ProBot server running on port 8080      ║")
    println("║  Open frontend/index.html in browser     ║")
    println("║  Press Ctrl+C to stop                    ║")
    println("╚══════════════════════════════════════════╝")

    // Block main thread, without this the JVM exits immediately
    Await.result(system.whenTerminated, scala.concurrent.duration.Duration.Inf)
  }
}

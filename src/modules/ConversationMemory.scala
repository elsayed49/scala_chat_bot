
package modules

import models.Models._

object ConversationMemory {

  
  def logInteraction(
    userInput   : String,
    botResponse : String,
    context     : ConversationState
  ): ConversationState = {

    val entry = InteractionEntry(
      sequenceNo     = context.history.length + 1,
      timestamp      = java.time.LocalDateTime.now().toString,
      userInput      = userInput,
      botResponse    = botResponse,
      detectedIntent = detectIntentFromInput(userInput).toString
    )

   
    context.copy(history = context.history :+ entry)
  }

  
  private def detectIntentFromInput(input: String): Intent = {
    val lower = input.toLowerCase
    lower match {
      case l if l.contains("recommend") || l.contains("suggest") => RecommendationReq
      case l if l.contains("what") || l.contains("explain")      => ExplanationReq
      case l if l.contains("prefer") || l.contains("like")       => PreferenceUpdate
      case l if l.contains("summary") || l.contains("summarize") => SummaryRequest
      case l if l.contains("hi") || l.contains("hello")          => Greeting
      case l if l.contains("quit") || l.contains("bye")          => ExitRequest
      case _                                                      => UnknownIntent
    }
  }

  
  def getConversationHistory(state: ConversationState): List[InteractionEntry] =
    state.history


  def getLastNInteractions(n: Int, state: ConversationState): List[InteractionEntry] =
    state.history.takeRight(n)

  
  def detectRepeatedQuery(input: String, history: List[InteractionEntry]): Boolean = {

    val inputWords = input.toLowerCase.split("\\s+").toList.filter(_.length > 3)

    
    history.exists { entry =>
      val entryWords = entry.userInput.toLowerCase.split("\\s+").toList
      val commonWords = inputWords.filter(w => entryWords.exists(e => e.contains(w)))
      commonWords.length >= 2
    }
  }

  
  def extractTopics(history: List[InteractionEntry]): List[String] = {

    
    val topics = history.flatMap { entry =>
      val lower = entry.userInput.toLowerCase

      
      lower match {
        case l if l.contains("pomodoro")                          => List("Pomodoro")
        case l if l.contains("deep work")                        => List("Deep Work")
        case l if l.contains("gtd") || l.contains("getting things") => List("GTD")
        case l if l.contains("time block")                       => List("Time Blocking")
        case l if l.contains("eisenhower")                       => List("Eisenhower Matrix")
        case l if l.contains("focus") || l.contains("concentrat") => List("Focus")
        case l if l.contains("energy")                           => List("Energy Management")
        case l if l.contains("priorit")                          => List("Prioritization")
        case l if l.contains("plan") || l.contains("schedule")   => List("Planning")
        case l if l.contains("recommend") || l.contains("suggest") => List("Recommendations")
        case l if l.contains("prefer")                           => List("Preferences")
        case _                                                   => List.empty
      }
    }

   
    topics.distinct
  }

  
  def summarizeConversation(history: List[InteractionEntry]): String = {

    
    history match {
      case Nil =>
        "No conversation history found yet. Start chatting to build your history!"

      case entries =>
        val totalInteractions = entries.length

        
        val recommendations = entries.filter(_.detectedIntent == "RecommendationReq").length
        val explanations    = entries.filter(_.detectedIntent == "ExplanationReq").length
        val preferences     = entries.filter(_.detectedIntent == "PreferenceUpdate").length

        
        val topics = extractTopics(entries)
        val topicsText = topics match {
          case Nil  => "no specific topics"
          case list => list.mkString(", ")
        }

        s"""
        | ── Conversation Summary ──────────────────────
        | Total interactions   : $totalInteractions
        | Recommendation requests: $recommendations
        | Explanation requests : $explanations
        | Preference updates   : $preferences
        | Topics discussed     : $topicsText
        | ──────────────────────────────────────────────
        """.stripMargin
    }
  }

  
  def getMostDiscussedTopics(history: List[InteractionEntry]): List[(String, Int)] = {

   
    val allTopics: List[String] = history.flatMap { entry =>
      val lower = entry.userInput.toLowerCase
      lower match {
        case l if l.contains("focus") || l.contains("concentrat") => List("Focus")
        case l if l.contains("energy")                            => List("Energy Management")
        case l if l.contains("priorit")                           => List("Prioritization")
        case l if l.contains("plan") || l.contains("schedule")    => List("Planning")
        case l if l.contains("recommend") || l.contains("suggest") => List("Recommendations")
        case l if l.contains("pomodoro")                          => List("Pomodoro")
        case l if l.contains("time block")                        => List("Time Blocking")
        case l if l.contains("eisenhower")                        => List("Eisenhower Matrix")
        case _                                                    => List.empty
      }
    }

    
    val grouped: Map[String, List[String]] = allTopics.groupBy(t => t)

    
    val counted: List[(String, Int)] = grouped.map {
      case (topic, occurrences) => (topic, occurrences.length)
    }.toList

   
    counted.sortBy { case (_, count) => -count }
  }

  
  def getUserMood(history: List[InteractionEntry]): String = {

    val positiveWords = List("great", "love", "amazing", "helpful", "good",
                             "excellent", "fantastic", "thanks", "perfect", "awesome")
    val negativeWords = List("boring", "bad", "useless", "hate", "dont like",
                             "not helpful", "confusing", "difficult", "stuck", "frustrated")

    
    val positiveCount = history.filter { entry =>
      positiveWords.exists(w => entry.userInput.toLowerCase.contains(w))
    }.length

    val negativeCount = history.filter { entry =>
      negativeWords.exists(w => entry.userInput.toLowerCase.contains(w))
    }.length

    
    (positiveCount, negativeCount) match {
      case (p, n) if p > n && p > 0 => "positive"
      case (p, n) if n > p && n > 0 => "negative"
      case (p, n) if p == n && p > 0 => "neutral"
      case _                         => "neutral"
    }
  }

  
  def handleSummaryRequest(tag: String, state: ConversationState): String = {

    tag match {

      case "[SUMMARY_REQUEST]" =>
        summarizeConversation(state.history)

      case "[ANALYSIS_REQUEST]" =>
        val mood   = getUserMood(state.history)
        val topics = getMostDiscussedTopics(state.history)

        // Pattern matching on mood
        val moodMessage = mood match {
          case "positive" => "You seem to be enjoying our session! Keep it up."
          case "negative" => "I sense some frustration. I will do my best to help better."
          case _          => "You seem focused and neutral. Let us keep making progress."
        }

        val topicsText = topics match {
          case Nil  => "No specific topics detected yet."
          case list =>
            list.take(3).map { case (topic, count) =>
              s"  • $topic ($count time(s))"
            }.mkString("\n")
        }

        s"""
        | ── Conversation Analysis ─────────────────────
        | Mood detected: ${mood.capitalize}
        | $moodMessage
        |
        | Most discussed topics:
        $topicsText
        | ──────────────────────────────────────────────
        """.stripMargin

      case _ =>
        "I was unable to process your request. Please try again."
    }
  }
}



package modules

import models.Models._

object CoreChatbot {

  
  def greetUser(): String =
    """
    |╔══════════════════════════════════════════════════════╗
    |║       Welcome to the Productivity Techniques Bot     ║
    |╚══════════════════════════════════════════════════════╝
    |
    | Good day! I am your personal Productivity Assistant.
    |
    | Here is what I can assist you with:
    |  • Recommending productivity techniques tailored to your needs
    |  • Explaining methods such as Pomodoro, Deep Work, GTD, and more
    |  • Tracking your preferences and conversation history
    |  • Summarizing our conversation at any point
    |
    | To get started, you may type something like:
    |  → "recommend something for focus"
    |  → "what is Pomodoro?"
    |  → "my goal is focus"
    |  → "summarize our conversation"
    |  → "quit" to exit
    |
    | How may I assist you today?
    """.stripMargin


  def normalizedInput(word:String):String = {
    word.replaceAll("(.)\\1+","$1")
  }

  def levenshtein(a: String, b: String): Int =
    (a.toList, b.toList) match {

      case (Nil, ys) => ys.length

      case (xs, Nil) => xs.length

      case (x :: xs, y :: ys) =>

        val cost =
          if (x == y) 0
          else 1

        List(
          levenshtein(xs.mkString, b) + 1,
          levenshtein(a, ys.mkString) + 1,
          levenshtein(xs.mkString, ys.mkString) + cost
        ).min
    }
  def parseInput(input: String): List[String] =
    input
      .toLowerCase
      .replaceAll("[^a-z0-9 ]", "")
      .split("\\s+")
      .toList
      .filter(_.nonEmpty)

  
  def containsAny(tokens: List[String], keywords: List[String]): Boolean =
    keywords.exists(kw => 
    tokens.exists( t => 
    levenshtein(
      normalizedInput(t),
      normalizedInput(kw)
    ) <= 1
    ))


  def detectIntent(tokens: List[String]): Intent =
    tokens match {

      
      case t if containsAny(t, List("quit", "exit", "bye", "goodbye", "stop","godbye"))
        => ExitRequest

      
      case t if containsAny(t, List("summary", "summarize", "recap","sumary","sumarize"))
        => SummaryRequest

      case t if containsAny(t, List("mood", "topics", "discussed", "analysis", "analyze","mod"))
        => AnalysisRequest

      
      case t if containsAny(t, List("recommend", "suggest", "give", "show","recomend"))
        => RecommendationReq

      
      case t if containsAny(t, List("what", "explain", "tell", "how", "describe", "define","tel"))
        => ExplanationReq

      
      case t if containsAny(t, List("prefer", "like", "love", "goal", "need","ned", "want", "update", "set", "beginner", "intermediate", "advanced", "short", "medium", "long", "focus", "energy", "plan", "priorit","beginer"))
        => PreferenceUpdate

      
      case t if containsAny(t, List("hi", "hello", "hey", "greetings","helo","gretings"))
        => Greeting

      case t if containsAny(t, List("pomodoro", "eisenhower", "kanban",
        "pareto", "gtd", "pomodoro", "deep",
        "blocking", "frog", "ivy", "smart",
        "moscow", "ultradian", "batch"))
      => ExplanationReq
      
      case _ => UnknownIntent
    }

  
  def generateResponse(query: String, state: ConversationState): String = {
    val tokens = parseInput(query)
    val intent = detectIntent(tokens)
    

    intent match {
      case Greeting          => generateGreetingResponse(state)
      case RecommendationReq => generateRecommendationPrompt(tokens, state)
      case ExplanationReq    => generateExplanationResponse(tokens)
      case PreferenceUpdate  => generatePreferenceUpdateResponse(tokens, state)
      case SummaryRequest    => "[SUMMARY_REQUEST]"
      case AnalysisRequest   => "[ANALYSIS_REQUEST]"
      case ExitRequest       => generateFarewellResponse(state)
      case UnknownIntent     => generateFallbackResponse(tokens)
    }
  }

  
  def handleUserInput(input: String, state: ConversationState): (String, Intent) = {

    val safeInput: Option[String] = Option(input).filter(_.trim.nonEmpty)

    safeInput match {
      case None =>
        ("I did not receive any input. Please type your query and press Enter.", UnknownIntent)

      case Some(userText) =>
        val tokens   = parseInput(userText)
        val intent   = detectIntent(tokens)
        val response = generateResponse(userText, state)
        (response, intent)
    }
  }

  

  private def generateGreetingResponse(state: ConversationState): String = {
    val sessionInfo = state.sessionCount match {
      case 1 => "It is a pleasure to meet you."
      case 2 => "Welcome back! Great to see you again."
      case n => s"Welcome back! This is session $n."
    }
    s"""
    | $sessionInfo
    | How may I assist you today? You may ask me to:
    |   • Recommend a productivity technique
    |   • Explain a specific method
    |   • Update your preferences
    |   • Summarize our conversation
    """.stripMargin
  }

  private def generateRecommendationPrompt(tokens: List[String], state: ConversationState): String = {

    // ✅ Pattern matching to detect goal from tokens
    val goalHint: Option[String] = tokens match {
      case t if containsAny(t, List("focus", "concentrate", "distraction"))
        => Some("focus")
      case t if containsAny(t, List("time", "schedule", "plan", "organiz"))
        => Some("time_management")
      case t if containsAny(t, List("energy", "motivation", "burnout", "tired"))
        => Some("energy_management")
      case t if containsAny(t, List("priorit", "important", "urgent", "task"))
        => Some("prioritization")
      case t if containsAny(t, List("planning", "kanban", "mind", "smart"))
        => Some("planning")
      case _ => None
    }

    // ✅ Pattern matching on Option
    goalHint match {
      case Some(goal) =>
        s"""
        | I understand you are looking for assistance with ${goal.replace("_", " ")}.
        | [RECOMMENDATION_REQUEST:$goal]
        """.stripMargin

      case None =>
        // Check if preferences already have a goal
        state.preferences.get("goal") match {
          case Some(goal) =>
            s"""
            | I will use your saved goal: ${goal.replace("_", " ")}.
            | [RECOMMENDATION_REQUEST:$goal]
            """.stripMargin
          case None =>
            """
            | I would be happy to recommend a technique!
            | Could you tell me your goal?
            |   • "recommend something for focus"
            |   • "recommend something for energy"
            |   • "recommend something for time management"
            |   • "recommend something for prioritization"
            """.stripMargin
        }
    }
  }

  private def generateExplanationResponse(tokens: List[String]): String = {

    
    val technique: Option[String] = tokens match {
      case t if containsAny(t, List("pomodoro"))
        => Some("Pomodoro")
      case t if containsAny(t, List("deep")) && containsAny(t, List("work"))
        => Some("Deep Work")
      case t if containsAny(t, List("gtd")) || (containsAny(t, List("getting")) && containsAny(t, List("done")))
        => Some("Getting Things Done")
      case t if containsAny(t, List("time")) && containsAny(t, List("block"))
        => Some("Time Blocking")
      case t if containsAny(t, List("eisenhower"))
        => Some("Eisenhower Matrix")
      case t if containsAny(t, List("2")) && containsAny(t, List("minute"))
        => Some("2-Minute Rule")
      case t if containsAny(t, List("eat")) && containsAny(t, List("frog"))
        => Some("Eat The Frog")
      case t if containsAny(t, List("mind")) && containsAny(t, List("map"))
        => Some("Mind Mapping")
      case t if containsAny(t, List("kanban"))
        => Some("Kanban Board")
      case t if containsAny(t, List("pareto"))
        => Some("Pareto Principle")
      case t if containsAny(t, List("smart")) && containsAny(t, List("goal"))
        => Some("SMART Goals")
      case t if containsAny(t, List("ivy")) && containsAny(t, List("lee"))
        => Some("Ivy Lee Method")
      case _ => None
    }

    technique match {
      case Some(name) =>
        s"[EXPLANATION_REQUEST:$name]"
      case None =>
        """
        | I would be glad to explain a productivity technique.
        | Please specify which technique:
        |   • "What is Pomodoro?"
        |   • "Explain Deep Work"
        |   • "Tell me about Eisenhower Matrix"
        |   • "What is GTD?"
        |   • "How does Kanban work?"
        |   • "What is SMART Goals?"
        """.stripMargin
    }
  }

  private def generatePreferenceUpdateResponse(tokens: List[String], state: ConversationState): String = {

    
    val prefKey: Option[(String, String)] = tokens match {
      case t if containsAny(t, List("focus", "concentrate"))
        => Some("goal", "focus")
      case t if containsAny(t, List("energy", "motivation"))
        => Some("goal", "energy_management")
      case t if containsAny(t, List("plan", "planning"))
        => Some("goal", "planning")
      case t if containsAny(t, List("priorit"))
        => Some("goal", "prioritization")
      case t if containsAny(t, List("time", "schedule"))
        => Some("goal", "time_management")
      case t if containsAny(t, List("beginner"))
        => Some("difficulty", "beginner")
      case t if containsAny(t, List("intermediate"))
        => Some("difficulty", "intermediate")
      case t if containsAny(t, List("advanced"))
        => Some("difficulty", "advanced")
      case t if containsAny(t, List("short", "quick"))
        => Some("duration", "short")
      case t if containsAny(t, List("medium"))
        => Some("duration", "medium")
      case t if containsAny(t, List("long", "hour"))
        => Some("duration", "long")
      case t if containsAny(t, List("like", "love", "prefer"))
        => Some("liked", "yes")
      case _ => None
    }

    
    prefKey match {
      case Some((key, value)) =>
        s"""
        | [PREFERENCE_UPDATE:$key:$value]
        | Noted! I have updated your preference:
        |   ${key.capitalize} → ${value.replace("_", " ")}
        | Your future recommendations will reflect this.
        """.stripMargin

      case None =>
        """
        | I would like to update your preferences.
        | Could you clarify? For example:
        |   • "I prefer beginner techniques"
        |   • "My goal is focus"
        |   • "I like short sessions"
        |   • "I am an advanced user"
        """.stripMargin
    }
  }

  private def generateFarewellResponse(state: ConversationState): String = {
    val message = state.history.length match {
      case 0 => "It seems we did not get to interact much. I hope to assist you next time!"
      case n if n <= 5 => s"We covered $n interaction(s). I hope our session was useful!"
      case n => s"We had a productive session with $n interactions. Excellent engagement!"
    }
    s"""
    | $message
    | Thank you for using the Productivity Techniques Assistant.
    | Goodbye, and best of luck with your productivity journey!
    """.stripMargin
  }

  private def generateFallbackResponse(tokens: List[String]): String = {
    val hint: String = tokens match {
      case t if containsAny(t, List("pomodoro", "deep", "gtd", "kanban", "eisenhower", "pareto", "ivy"))
        => "It appears you may be asking about a technique. Try: \"What is [technique name]?\""
      case t if containsAny(t, List("help", "can", "able"))
        => "You may ask me to recommend a technique, explain a method, or update your preferences."
      case Nil
        => "Please provide a query and I will do my best to assist you."
      case t
        => s"I noticed: ${t.take(3).mkString(", ")}. Could you elaborate on what you need?"
    }
    s"""
    | I was unable to fully interpret your request.
    | $hint
    |
    | Examples:
    |   • "Recommend something for focus"
    |   • "What is the Pomodoro Technique?"
    |   • "I prefer beginner techniques"
    |   • "Summarize our conversation"
    """.stripMargin
  }
}

package models


object Models {

  
  case class Technique(
    name        : String,       // e.g. "Pomodoro Technique"
    category    : String,       // focus | time-management | energy | planning | prioritization
    duration    : Int,          // estimated duration in minutes
    difficulty  : String,       // beginner | intermediate | advanced
    description : String,       // full explanation of the technique
    tags        : List[String]  // e.g. List("timer", "breaks", "focus")
  )

  
  case class InteractionEntry(
    sequenceNo     : Int,     // turn number (1, 2, 3, ...)
    timestamp      : String,  // time of interaction (ISO string)
    userInput      : String,  // what the user typed
    botResponse    : String,  // what the bot replied
    detectedIntent : String   // classified intent (e.g. "recommendation_request")
  )

  
  case class ConversationState(
    history      : List[InteractionEntry],  // full conversation log
    preferences  : Map[String, String],     // user preferences (e.g. goal -> focus)
    sessionCount : Int                      // how many sessions the user has had
  )

  
  object ConversationState {
    def empty: ConversationState = ConversationState(
      history      = List.empty,
      preferences  = Map.empty,
      sessionCount = 1
    )
  }

 
  sealed trait Intent
  case object Greeting            extends Intent
  case object RecommendationReq   extends Intent
  case object ExplanationReq      extends Intent
  case object PreferenceUpdate    extends Intent
  case object SummaryRequest      extends Intent
  case object AnalysisRequest     extends Intent
  case object ExitRequest         extends Intent
  case object UnknownIntent       extends Intent
}
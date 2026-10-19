

package modules

import models.Models._
import data.KnowledgeBase.allTechniques

object RecommendationEngine {

  
  def getUserPreferences(state: ConversationState): Map[String, String] =
    state.preferences

  
  def updatePreferences(key: String, value: String, state: ConversationState): ConversationState = {
    val updatedPreferences = state.preferences + (key -> value)
    state.copy(preferences = updatedPreferences)
  }

  
  def scoreItem(technique: Technique, preferences: Map[String, String]): Int = {

    val goalScore: Int = preferences.get("goal") match {
      case Some(goal) if technique.category == goal    => 3
      case Some(goal) if technique.tags.contains(goal) => 1
      case _                                           => 0
    }

    val difficultyScore: Int = preferences.get("difficulty") match {
      case Some(diff) if technique.difficulty == diff  => 2
      case _                                           => 0
    }

    val durationScore: Int = preferences.get("duration") match {
      case Some("short")  if technique.duration <= 15  => 2
      case Some("medium") if technique.duration <= 30  => 2
      case Some("long")   if technique.duration >= 60  => 2
      case Some("short")  if technique.duration <= 30  => 1
      case Some("medium") if technique.duration <= 60  => 1
      case _                                           => 0
    }

    goalScore + difficultyScore + durationScore
  }

  
  def recommend(preferences: Map[String, String], data: List[Technique]): Option[List[Technique]] = {

    
    val scored: List[(Technique, Int)] =
      data.map(t => (t, scoreItem(t, preferences)))

    
    val matched: List[(Technique, Int)] =
      scored.filter { case (_, score) => score > 0 }

    
    val ranked: List[(Technique, Int)] =
      matched.sortBy { case (_, score) => -score }

   
    val topThree: List[Technique] =
      ranked.take(3).map { case (technique, _) => technique }

    
    topThree match {
      case Nil  => None
      case list => Some(list)
    }
  }

  
  def explainRecommendation(technique: Technique, preferences: Map[String, String]): String = {

    val goalReason: String = preferences.get("goal") match {
      case Some(goal) if technique.category == goal =>
        s"It directly targets your goal of ${goal.replace("_", " ")}."
      case Some(goal) if technique.tags.contains(goal) =>
        s"It is related to your interest in ${goal.replace("_", " ")}."
      case _ =>
        "It is a highly versatile technique suitable for most goals."
    }

    val difficultyReason: String = preferences.get("difficulty") match {
      case Some(diff) if technique.difficulty == diff =>
        s"It matches your preferred difficulty level: $diff."
      case Some(_) =>
        s"It is a ${technique.difficulty}-level technique."
      case None =>
        s"It is suitable for ${technique.difficulty} practitioners."
    }

    val durationReason: String = preferences.get("duration") match {
      case Some("short")  =>
        s"At only ${technique.duration} minutes, it fits your preference for short sessions."
      case Some("medium") =>
        s"At ${technique.duration} minutes, it fits your preference for medium sessions."
      case Some("long")   =>
        s"At ${technique.duration} minutes, it suits your preference for longer sessions."
      case _ =>
        s"It requires approximately ${technique.duration} minutes to complete."
    }

    s"""
    | ✦ ${technique.name}
    | $goalReason
    | $difficultyReason
    | $durationReason
    """.stripMargin
  }

  
  def explainTechnique(name: String): Option[String] = {

    val found: Option[Technique] =
      allTechniques.find(t => t.name.toLowerCase.contains(name.toLowerCase))

    found match {
      case None => None
      case Some(technique) =>
        Some(
          s"""
          |┌─────────────────────────────────────────┐
          | ${technique.name}
          |└─────────────────────────────────────────┘
          | Category  : ${technique.category.replace("_", " ").capitalize}
          | Duration  : ${technique.duration} minutes
          | Difficulty: ${technique.difficulty.capitalize}
          |
          | Description:
          | ${technique.description}
          |
          |
          | Tags: ${technique.tags.mkString(", ")}
          """.stripMargin
        )
    }
  }

  
  def handleRecommendationRequest(tag: String, state: ConversationState): String = {

    tag match {

      case t if t.startsWith("[RECOMMENDATION_REQUEST:") =>
        val goal        = t.replace("[RECOMMENDATION_REQUEST:", "").replace("]", "")
        val updatedState = updatePreferences("goal", goal, state)
        val preferences  = getUserPreferences(updatedState)
        val result       = recommend(preferences, allTechniques)

        result match {
          case None =>
            """
            | No techniques found matching your preferences.
            | Try broadening your preferences and trying again.
            """.stripMargin

          case Some(techniques) =>
            val formatted = techniques
              .zipWithIndex
              .map { case (technique, index) =>
                s"""
                | ${index + 1}. ${explainRecommendation(technique, preferences)}
                |    About: ${technique.description.take(80)}...
                """.stripMargin
              }
              .mkString("\n")

            s"""
            | Based on your preference for ${goal.replace("_", " ")},
            | here are your top 3 recommended techniques:
            |
            $formatted
            | Would you like me to explain any of these in more detail?
            """.stripMargin
        }

      case t if t.startsWith("[EXPLANATION_REQUEST:") =>
        val name = t.replace("[EXPLANATION_REQUEST:", "").replace("]", "")
        explainTechnique(name) match {
          case Some(explanation) => explanation
          case None =>
            s"""
            | I could not find a technique named "$name".
            | Try: Pomodoro, Deep Work, Getting Things Done,
            | Time Blocking, Eisenhower Matrix, 2-Minute Rule,
            | Eat The Frog, Mind Mapping, Kanban, SMART Goals.
            """.stripMargin
        }

      case t if t.startsWith("[PREFERENCE_UPDATE:") =>
        val parts = t.replace("[PREFERENCE_UPDATE:", "").replace("]", "").split(":")
        parts match {
          case Array(key, value) =>
            s"""
            | Your preference has been saved:
            |   ${key.capitalize} → ${value.replace("_", " ")}
            | Would you like recommendations based on your preferences?
            """.stripMargin
          case _ =>
            "I was unable to process the preference update. Please try again."
        }

      case _ =>
        """
        | I was unable to process your request.
        | Please ask for a recommendation or explain a technique.
        """.stripMargin
    }
  }

  
  def getAllCategories(): List[String] =
    allTechniques.map(_.category).distinct.sorted

  
  def getTechniquesByCategory(category: String): Option[List[Technique]] = {
    val filtered = allTechniques.filter(_.category == category)
    filtered match {
      case Nil  => None
      case list => Some(list)
    }
  }
}
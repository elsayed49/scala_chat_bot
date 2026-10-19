

import models.Models._
import modules.CoreChatbot._
import modules.RecommendationEngine._
import modules.ConversationMemory._
import data.KnowledgeBase.allTechniques

object Main extends App {

  
  val GREEN  = "\u001B[32m"
  val BLUE   = "\u001B[34m"
  val YELLOW = "\u001B[33m"
  val CYAN   = "\u001B[36m"
  val RED    = "\u001B[31m"
  val BOLD   = "\u001B[1m"
  val RESET  = "\u001B[0m"

  
  val sessionStart = System.currentTimeMillis()

  
  val techniqueOfTheDay = allTechniques(
    scala.util.Random.nextInt(allTechniques.length)
  )

  
  println(s"${CYAN}${BOLD}")
  println("╔══════════════════════════════════════════════════════╗")
  println("║       Welcome to the Productivity Techniques Bot     ║")
  println("╚══════════════════════════════════════════════════════╝")
  println(RESET)

  print(s"${YELLOW}Please enter your name: ${RESET}")
  val userName = scala.io.StdIn.readLine().trim match {
    case ""   => "Friend"
    case name => name
  }

  
  var state = ConversationState(
    history      = List.empty,
    preferences  = Map("userName" -> userName),
    sessionCount = 1
  )

  
  println(s"\n${GREEN}${BOLD}Bot:${RESET} ${GREEN}Welcome, $userName! ${RESET}")
  println(greetUser())

  println(s"${YELLOW}${BOLD}💡 Technique of the Day:${RESET}")
  println(s"${YELLOW}   ${techniqueOfTheDay.name} — ${techniqueOfTheDay.description.take(80)}...${RESET}")
  println(s"${YELLOW}   Duration: ${techniqueOfTheDay.duration} min | Difficulty: ${techniqueOfTheDay.difficulty}${RESET}\n")

  
  var running      = true
  var interactionCount = 0

  while (running) {

    print(s"${BLUE}${BOLD}$userName: ${RESET}")
    val userInput = scala.io.StdIn.readLine()

    
    Option(userInput).filter(_.trim.nonEmpty) match {

      case None =>
        println(s"\n${GREEN}${BOLD}Bot:${RESET} I did not receive any input. Please type your query.\n")

      case Some(input) =>

        
        val (response, intent) = handleUserInput(input, state)
        interactionCount += 1

        intent match {

          
          case ExitRequest =>
            val duration    = (System.currentTimeMillis() - sessionStart) / 1000
            val minutes     = duration / 60
            val seconds     = duration % 60
            println(s"\n${GREEN}${BOLD}Bot:${RESET} ${GREEN}$response${RESET}")
            println(s"${CYAN}── Session Summary for $userName ────────────────${RESET}")
            println(s"${CYAN}   Duration       : ${minutes}m ${seconds}s${RESET}")
            println(s"${CYAN}   Total exchanges: $interactionCount${RESET}")
            println(s"${CYAN}   Techniques explored: ${extractTopics(state.history).length}${RESET}")
            println(s"${CYAN}────────────────────────────────────────────────${RESET}")
            running = false

          
          case SummaryRequest =>
            // LEVEL 3: Has history?
            state.history match {
              case Nil =>
                val msg = s"We have not had any conversation yet, $userName. Start chatting first!"
                println(s"\n${GREEN}${BOLD}Bot:${RESET} $msg\n")
                state = logInteraction(input, msg, state)

              case _ =>
                val summaryResponse = handleSummaryRequest("[SUMMARY_REQUEST]", state)
                println(s"\n${GREEN}${BOLD}Bot:${RESET} ${GREEN}$summaryResponse${RESET}\n")
                state = logInteraction(input, summaryResponse, state)
            }

          
          case AnalysisRequest =>
            val analysisResponse = handleSummaryRequest("[ANALYSIS_REQUEST]", state)
            println(s"\n${GREEN}${BOLD}Bot:${RESET} ${GREEN}$analysisResponse${RESET}\n")
            state = logInteraction(input, analysisResponse, state)

          
          case Greeting =>
            
            state.history.length match {
              case 0 =>
                val msg = s"Hello $userName! I am glad you are here. How can I assist you today?"
                println(s"\n${GREEN}${BOLD}Bot:${RESET} ${GREEN}$msg${RESET}\n")
                state = logInteraction(input, msg, state)
              case n =>
                val msg = s"Hello again $userName! We have had $n interactions so far. What would you like to explore next?"
                println(s"\n${GREEN}${BOLD}Bot:${RESET} ${GREEN}$msg${RESET}\n")
                state = logInteraction(input, msg, state)
            }

          
          case _ =>

            
            val finalResponse = response match {

              
              case r if r.contains("[RECOMMENDATION_REQUEST:") =>
                val tag  = extractTag(r, "[RECOMMENDATION_REQUEST:")
                val goal = tag.replace("[RECOMMENDATION_REQUEST:", "").replace("]", "")

                
                state.preferences.get("goal") match {
                  case Some(existingGoal) if existingGoal == goal =>
                    handleRecommendationRequest(tag, state)
                  case _ =>
                    state = updatePreferences("goal", goal, state)
                    handleRecommendationRequest(tag, state)
                }

              
              case r if r.contains("[EXPLANATION_REQUEST:") =>
                val tag  = extractTag(r, "[EXPLANATION_REQUEST:")
                val name = tag.replace("[EXPLANATION_REQUEST:", "").replace("]", "")

                
                explainTechnique(name) match {
                  case Some(_) =>
                    handleRecommendationRequest(tag, state)
                  case None =>
                    s"I could not find $name. Try: Pomodoro, Deep Work, Eisenhower Matrix, GTD, Kanban."
                }

              
              case r if r.contains("[PREFERENCE_UPDATE:") =>
                val tag   = extractTag(r, "[PREFERENCE_UPDATE:")
                val parts = tag.replace("[PREFERENCE_UPDATE:", "").replace("]", "").split(":")
                parts match {
                  case Array(key, value) =>
                    state = updatePreferences(key, value, state)
                    handleRecommendationRequest(tag, state)
                  case _ =>
                    "I was unable to process your preference. Please try again."
                }

              case r => r
            }

            
            val repeatedNote = detectRepeatedQuery(input, state.history) match {
              case true  =>
                s"\n${YELLOW}(Note: $userName, you asked about this before. Here is my response again.)${RESET}"
              case false => ""
            }

            
            val moodPrefix = getUserMood(state.history) match {
              case "negative" =>
                s"${RED}I sense some frustration, $userName. Let me try to help better.${RESET}\n"
              case "positive" =>
                s"${GREEN}Great energy, $userName! Let us keep going.${RESET}\n"
              case _ => ""
            }

            println(s"\n${GREEN}${BOLD}Bot:${RESET} $moodPrefix$repeatedNote${GREEN}$finalResponse${RESET}\n")
            state = logInteraction(input, finalResponse, state)

            
            if (interactionCount % 5 == 0) {
              val topics = extractTopics(state.history)
              val mood   = getUserMood(state.history)
              println(s"${CYAN}── Progress Update for $userName ─────────────────${RESET}")
              println(s"${CYAN}   Interactions so far : $interactionCount${RESET}")
              println(s"${CYAN}   Topics explored     : ${topics.mkString(", ")}${RESET}")
              println(s"${CYAN}   Current mood        : ${mood.capitalize}${RESET}")
              println(s"${CYAN}──────────────────────────────────────────────────${RESET}\n")
            }
        }
    }
  }

  
  def extractTag(response: String, tagStart: String): String = {
    val startIndex = response.indexOf(tagStart)
    val endIndex   = response.indexOf("]", startIndex)
    if (startIndex >= 0 && endIndex >= 0)
      response.substring(startIndex, endIndex + 1)
    else
      ""
  }
}
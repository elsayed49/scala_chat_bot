package data

import models.Models.Technique
import scala.io.Source

object KnowledgeBase {

  val allTechniques: List[Technique] = {

    val lines =
      Source
        .fromFile("src/data/techniques.csv")
        .getLines()
        .drop(1)
        .toList

    lines.map { line =>

      val parts = {
        line.split(",(?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)")

        /*
        Array(
          "Pomodoro Technique",
          "focus",
          "25",
          "beginner",
          "\"Work in intervals\"",
          "\"timer|breaks|focus\""
        )
        */
      }
      Technique(
        name        = parts(0),
        category    = parts(1),
        duration    = parts(2).toInt,
        difficulty  = parts(3),
        description = parts(4).replace("\"", ""),
        tags        =
          parts(5)
            .replace("\"", "")
            .split("\\|")
            .toList
      )
    }
  }
}
scalaVersion := "3.3.7"

name := "productivity-chatbot"
version := "0.1.0"

libraryDependencies ++= Seq(
  "org.apache.pekko" %% "pekko-http"            % "1.1.0",
  "org.apache.pekko" %% "pekko-http-spray-json" % "1.1.0",
  "org.apache.pekko" %% "pekko-actor-typed"     % "1.1.0",
  "org.apache.pekko" %% "pekko-stream"          % "1.1.0"
)

Compile / scalaSource := baseDirectory.value / "src"
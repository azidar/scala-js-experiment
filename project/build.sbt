
addSbtPlugin("org.scala-lang.modules.scalajs" % "scalajs-sbt-plugin" % "0.5.3")

resolvers += "spray repo" at "http://repo.spray.io"

resolvers  += "Online Play Repository" at
  "http://repo.typesafe.com/typesafe/simple/maven-releases/"

addSbtPlugin("com.lihaoyi" % "workbench" % "0.1.5")

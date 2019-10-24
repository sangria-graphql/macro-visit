resolvers += Resolver.url(
  "typesafe sbt-plugins",
  url("https://dl.bintray.com/typesafe/sbt-plugins")
)(Resolver.ivyStylePatterns)

addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "0.6.1")

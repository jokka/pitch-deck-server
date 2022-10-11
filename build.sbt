lazy val root = (project in file("."))
  .settings(
    name           := "pitch-deck-server",
    version        := "1.0-SNAPSHOT",
    publish / skip := true,
    scalaVersion   := "2.13.9",
    scalacOptions += "-Ymacro-annotations",
    tpolecatExcludeOptions += ScalacOptions.lintPackageObjectClasses,
    addCompilerPlugin("org.typelevel" % "kind-projector" % "0.13.2" cross CrossVersion.full),
    libraryDependencies ++= Seq(
      "ch.qos.logback"    % "logback-classic"     % "1.4.3",
      "co.fs2"           %% "fs2-core"            % "3.3.0",
      "co.fs2"           %% "fs2-io"              % "3.3.0",
      "com.chuusai"      %% "shapeless"           % "2.3.10",
      "com.comcast"      %% "ip4s-core"           % "3.2.0",
      "io.circe"         %% "circe-core"          % "0.14.3",
      "io.circe"         %% "circe-generic"       % "0.14.3",
      "org.apache.pdfbox" % "pdfbox"              % "2.0.27",
      "org.http4s"       %% "http4s-circe"        % "0.23.16",
      "org.http4s"       %% "http4s-core"         % "0.23.16",
      "org.http4s"       %% "http4s-dsl"          % "0.23.16",
      "org.http4s"       %% "http4s-ember-server" % "0.23.16",
      "org.http4s"       %% "http4s-server"       % "0.23.16",
      "org.typelevel"    %% "cats-core"           % "2.8.0",
      "org.typelevel"    %% "cats-effect"         % "3.3.14",
      "org.typelevel"    %% "cats-effect-kernel"  % "3.3.14",
      "org.typelevel"    %% "cats-effect-std"     % "3.3.14",
      "org.typelevel"    %% "log4cats-slf4j"      % "2.5.0"
    )
  )

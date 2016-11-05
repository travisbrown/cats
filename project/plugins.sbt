addSbtPlugin("com.eed3si9n"         % "sbt-unidoc"            % "0.3.3")
addSbtPlugin("com.github.gseitz"    % "sbt-release"           % "1.0.3")
addSbtPlugin("com.jsuereth"         % "sbt-pgp"               % "1.0.0")
addSbtPlugin("com.typesafe"         % "sbt-mima-plugin"       % "0.1.11")
addSbtPlugin("com.typesafe.sbt"     % "sbt-ghpages"           % "0.5.3")
addSbtPlugin("pl.project13.scala"   % "sbt-jmh"               % "0.2.16")
addSbtPlugin("org.scalastyle"      %% "scalastyle-sbt-plugin" % "0.8.0")
addSbtPlugin("org.scoverage"        % "sbt-scoverage"         % "1.5.0-RC2")
addSbtPlugin("com.typesafe.sbt"     % "sbt-git"               % "0.8.5")
addSbtPlugin("org.scala-js"         % "sbt-scalajs"           % "0.6.13")
addSbtPlugin("com.github.tkawachi"  % "sbt-doctest"           % "0.4.1")
addSbtPlugin("org.xerial.sbt"       % "sbt-sonatype"          %  "1.1")
addSbtPlugin("com.fortysevendeg"    % "sbt-microsites"        % "0.3.0")

// Tut is transitively pulled in by sbt-microsites, but as of 0.3.0 that
// currently results in the wrong version (0.4.5 instead of 0.4.6).
addSbtPlugin("org.tpolecat"         % "tut-plugin"            % "0.4.6")

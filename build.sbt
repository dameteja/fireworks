course := "effective-scala"
assignment := "fireworks"

scalaVersion := "3.5.2"

libraryDependencies ++= Seq(
  ("org.creativescala" %% "doodle"           % "0.10.0").cross(CrossVersion.for3Use2_13),
  "org.scalameta"      %% "munit"            % "0.7.26"  % Test,
  "org.scalacheck"     %% "scalacheck"       % "1.15.4"  % Test
)

Compile / scalacOptions ++= Seq("-deprecation")

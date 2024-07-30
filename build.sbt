ThisBuild / crossScalaVersions := Seq("2.13.14", "2.12.19")
ThisBuild / scalaVersion := (ThisBuild / crossScalaVersions).value.head
ThisBuild / organization := "com.dwolla"
ThisBuild / homepage := Some(url("https://github.com/Dwolla/jdbc-testutils"))
ThisBuild / licenses += ("MIT", url("https://opensource.org/licenses/MIT"))
ThisBuild / developers := List(
  Developer(
    "bpholt",
    "Brian Holt",
    "bholt+jdbc-testutils@dwolla.com",
    url("https://dwolla.com")
  ),
)
ThisBuild / startYear := Option(2024)
ThisBuild / tlJdkRelease := Option(8)
ThisBuild / githubWorkflowJavaVersions := Seq(JavaSpec.temurin("17"))
ThisBuild / tlCiReleaseBranches := Seq("main")
ThisBuild / tlBaseVersion := "0.1"
ThisBuild / tlSonatypeUseLegacyHost := true
ThisBuild / mergifyStewardConfig ~= { _.map {
  _.withAuthor("dwolla-oss-scala-steward[bot]")
    .withMergeMinors(true)
}}
ThisBuild / mergifySuccessConditions += MergifyCondition.Custom("#approved-reviews-by>=1")

lazy val `jdbc-testutils` = project
  .in(file("."))
  .settings(
    description := "JDBC Test Utilities",
    libraryDependencies ++= {
      val munitVersion = "1.0.0"

      Seq(
        "com.chuusai" %% "shapeless" % "2.3.12",
        "com.h2database" % "h2" % "2.2.224",
        "org.scalameta" %% "munit" % munitVersion % Test,
        "org.scalameta" %% "munit-scalacheck" % munitVersion % Test,
      ) ++
        Option(scalaVersion.value)
          .filter(_.startsWith("2.12"))
          .map(_ => "org.scala-lang" % "scala-reflect" % scalaVersion.value)
          .toList
    },
  )

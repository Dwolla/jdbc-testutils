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
  )

import sbt._
import Keys._

object Programming1 {
  val COMMON = Seq(
      ThisBuild / version      := "0.1.0",
      ThisBuild / organization := "jp.ac.titech.is.prg1",

      run / fork          := true,
      run / connectInput  := true,
      Global / cancelable := true,
    )

  val SCALA3 = Seq(
      ThisBuild / scalaVersion := "3.2.2",                   // scalac コンパイラのバージョン
      Compile / scalaSource := baseDirectory.value / "src",  // Scala のソース置き場のディレクトリ
      scalacOptions := Seq(
        //"-explain",
        "-Werror",                                           // 警告をエラーとして扱う
      ),
    )

  val SCALA_SWING = Seq(
    libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "3.0.0",
  )

  val PARALLE__COLLECTIONS = Seq(
    //libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "0.2.0",
    libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4",
    trapExit := false,
  )

  val Scala3 = COMMON ++ SCALA3
  val ScalaSwing3 = Scala3 ++ SCALA_SWING
}


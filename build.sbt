import android.Keys._
import scala.collection.JavaConversions._

name := "play-services-games-before-sign-in"

version := "1.0.2-RC3"

scalaVersion := "2.11.5"

organization := "nl.cunodeboer.commons.android"

publishMavenStyle := true

resolvers += Resolver.mavenLocal

javacOptions ++= Seq("-source", "1.8", "-target", "1.8")

platformTarget in Android := "android-21"

minSdkVersion := "15"

targetSdkVersion := "21"

android.Plugin.buildAar

libraryDependencies ++= Seq(
  // PROJECT DEPS
  "com.google.android.gms" % "play-services-games" % "7.0.+",
  "com.typesafe.play" %% "play-json" % "2.4.+",
  // LOGGING DEPS
  "org.clapper" %% "grizzled-slf4j" % "1.+",
  "ch.qos.logback" % "logback-classic" % "1.+",
  // TEST DEPS
  "org.scalatest" %% "scalatest" % "2.1.7" % "test",
  "org.mockito" % "mockito-core" % "1.10.8" % "test"
)

unmanagedClasspath in Test ++= (builder in Android).value.getBootClasspath map Attributed.blank
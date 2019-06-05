import sbt._

object Dependencies {
  lazy val cats         = "org.typelevel" %% "cats-core"      % "1.6.1"
  lazy val catsEffect   = "org.typelevel" %% "cats-effect"    % "1.1.0"
  lazy val zio          = "org.scalaz"    %% "scalaz-zio"     % "1.0-RC5"
  lazy val fs2          = "co.fs2"        %% "fs2-io"         % "1.0.0"
  lazy val breeze       = "org.scalanlp"  %% "breeze"         % "0.13.2"
  lazy val breezeNative = "org.scalanlp"  %% "breeze-natives" % "0.13.2"
  lazy val singletonOps = "eu.timepit"    %% "singleton-ops"  % "0.3.1"

  lazy val scalaTest    = "org.scalatest" %% "scalatest"      % "3.0.5"
  lazy val catsTestKit  = "org.typelevel" %% "cats-testkit"   % "1.6.1"


}
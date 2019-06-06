package io.tuliplogic.raytracer.zibaldone

import io.tuliplogic.linalg.breezematrix
import scalaz.zio.stream.ZStream
import scalaz.zio.{App, UIO, ZIO, console}
import scalaz.zio.console.Console

/**
  *
  * ray-tracer - 2019-06-05
  * Created with ♥ in Amsterdam
  */
object balistics extends App {

  import breezematrix.{doubleMatrixAlgebra => alg}

  import breezematrix.Mat

  type Vec = Mat[Double, 3, 1]

  case class Env(gravity: Vec, wind: Vec)
  case class Projectile(pos: Vec, vel: Vec)
  trait EnvProvider {
    val env: Env
  }

  def tick(projectile: Projectile): ZIO[EnvProvider, Nothing, Projectile] = for {
    newPos  <- UIO.succeed(alg.plus(projectile.pos, projectile.vel))
    envProvider <- ZIO.environment[EnvProvider]
    newVel <- UIO.succeed(alg.plus(alg.plus(projectile.vel, envProvider.env.gravity), envProvider.env.wind))
  } yield Projectile(newPos, newVel)

  def shoot(initialPos: Vec, initialVel: Vec): ZStream[EnvProvider, Nothing, Projectile] =
    ZStream.unfoldM(Projectile(initialPos, initialVel)) { p =>
      tick(p).map( p => Some((p, p)) )
    }

  def runExperiment(initialPos: Vec, initialVel: Vec): ZIO[Console with EnvProvider, Nothing, Unit] = {
    console.putStrLn(s"running experiment with initial pos: $initialPos, vel: $initialVel") *>
    shoot(initialPos, initialVel)
      .takeWhile(p => alg.elem(p.pos)(1) > 0)
      .foreach(p => console.putStrLn(s"Pos(${alg.elem(p.pos)(0)}, ${alg.elem(p.pos)(1)}, ${alg.elem(p.pos)(2)}"))
  }

  override def run(args: List[String]): ZIO[balistics.Environment, Nothing, Int] = {

    val environment: Env = Env(alg.createCol[3](0, -0.1, 0), alg.createCol[3](0, -0.1, 0))

    trait LiveEnvProvider extends EnvProvider {
      override val env: Env = environment
    }

    val program: ZIO[Console with EnvProvider, Throwable, Unit] = for {
      _ <- console.putStrLn("Initial position (0, 1, 0)")
      pos <- UIO.succeed(alg.create[3, 1](0, 1, 0))

      _   <- console.putStrLn("v_x:")
      v_x_raw <- console.getStrLn
      v_x <- ZIO.effect(v_x_raw.toDouble)
      _   <- console.putStrLn("v_y:")
      v_y_raw <- console.getStrLn
      v_y <- ZIO.effect(v_y_raw.toDouble)
      _   <- console.putStrLn("v_z:")
      v_z_raw <- console.getStrLn
      v_z <- ZIO.effect(v_z_raw.toDouble)
      vel <- ZIO.succeed(alg.create[3, 1](v_x, v_y, v_z))
      _ <- runExperiment(pos, vel)
    } yield ()

    program.provideSome[Console] { console_ =>
        new Console with LiveEnvProvider {
          val console: Console.Service[Any] =  console_.console
        }
      }.fold(_ => 1, _ => 0)
  }

}

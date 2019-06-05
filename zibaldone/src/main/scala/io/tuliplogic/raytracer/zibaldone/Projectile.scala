package io.tuliplogic.raytracer.zibaldone

import cats.Id
import cats.data.{Kleisli, Reader}
import io.tuliplogic.linalg.breezematrix
import cats.implicits._

/**
  *
  * ray-tracer - 2019-06-05
  * Created with ♥ in Amsterdam
  */
object balistics {

  import breezematrix.{doubleMatrixAlgebra => alg}

  import breezematrix.Mat

  type Vec = Mat[Double, 2, 1]

  case class Env(gravity: Vec, wind: Vec)
  case class Projectile(pos: Vec, vel: Vec)

  def tick(projectile: Projectile): Reader[Env, Projectile] = for {
    newPos  <- alg.plus(projectile.pos, projectile.vel).pure[Reader[Env, ?]]
    env <- Kleisli.ask[Id, Env]
    newVel <- alg.plus(alg.plus(projectile.vel, env.gravity), env.wind).pure[Reader[Env, ?]]
  } yield Projectile(newPos, newVel)
}

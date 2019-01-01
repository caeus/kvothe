package io.plutus

import io.plutus.PackerResult.{Done, Failed}


trait PackerResult[In, Out]
object PackerResult {
  case class Failed[In, Out]() extends PackerResult[In, Out]
  case class Done[In, Out](
    value: Out,
    remaining: List[In]
  ) extends PackerResult[In, Out]
  //Sorry byt later case class Cont[In, Out](next: Tarser[In, Out]) extends TarserResult[In, Out]
  //Adding this makes it very difficult to add the forking ability
}
trait Packer[In, Out] {

  def take(input: List[In]): PackerResult[In, Out]

}

object Packer {

  object syntax {
    implicit class TarserOps[In, Out](val value: Packer[In, Out]) extends AnyVal {
      def map[Out1](func: Out => Out1): Packer[In, Out1] = mapResult(value)(func)

      def ~[Out1](next: => Packer[In, Out1]): Packer[In, Unit] = mapResult(concat(value, next))(_ => ())

      def ~>[Out1](next: => Packer[In, Out1]): Packer[In, Out1] = mapResult(concat(value, next))(_._2)

      def <~>[Out1](next: => Packer[In, Out1]): Packer[In, (Out,Out1)] = concat(value, next)

      def <~[Out1](next: => Packer[In, Out1]): Packer[In, Out] = mapResult(concat(value, next))(_._1)

      def ? : Packer[In, Option[Out]] = mapResult(repeat(value, 0, Some(1)))(_.headOption)

      def rep(min: Int = 0, max: Option[Int] = None): Packer[In, List[Out]] = {
        repeat(value, min, max)
      }

      def ! : Packer[In, List[In]] = capture(value)

      def |(other: => Packer[In, Out]): Packer[In, Out] = fork(List(value, other))
    }
  }
  def capture[In, Out](value: Packer[In, Out]): Packer[In, List[In]] = {
    input: List[In] =>
      value.take(input) match {
        case Done(_, remaining) => Done(input.take(input.size - remaining.size), remaining)
        case Failed() => Failed()
      }
  }

  private def repeatRec[In, Out](
    elem: Packer[In, Out],
    min: Int,
    max: Option[Int],
    accum: List[Out],
  ): Packer[In, List[Out]] = {
    (min compareTo 0, max.map(_ compareTo min).getOrElse(1)) match {
      case (-1, _) => throw new IllegalArgumentException("Repeat qty must be greater than or equals than zero")
      case (_, -1) => throw new IllegalArgumentException("Repeat qty must be greater than or equals than zero")
      case (0, 0) =>
        pure(accum)
      case (_, 1) => //minimum is zero, and it can go on!
        (input: List[In]) => {
          elem.take(input) match {
            case Failed() =>
              if (min == 0)
                Done(accum, input)
              else Failed()
            case Done(value, remaining) =>
              repeatRec(elem, 0, max.map(_ - 1), value :: accum)
                .take(remaining)
          }
        }
    }
  }

  def repeat[In, Out](
    elem: Packer[In, Out],
    min: Int,
    max: Option[Int]
  ): Packer[In, List[Out]] = {
    mapResult(repeatRec(elem, min, max, Nil))(_.reverse)
  }


  def pure[In, Out](value: Out): Packer[In, Out] = new PurePacker[In, Out](value)


  //def fromPredicate[In](pred: In=>Boolean):Tarser[]
  private class FromListPacker[In](from: List[In]) extends Packer[In, Unit] {
    def reqTake(
      input: List[In],
      recFrom: List[In],
      recInput: List[In]
    ): PackerResult[In, Unit] = {
      (recFrom, recInput) match {
        case (h1 :: t1, h2 :: t2) if h1 == h2 =>
          reqTake(input, t1, t2)
        case (_ :: _, _) =>
          Failed()
        case (Nil, _) =>
          Done((), recInput)
      }
    }

    override def take(input: List[In]): PackerResult[In, Unit] = {
      println(input -> s"fromList for $from")
      reqTake(input, from, input)
    }
  }

  def fromList[In](from: List[In]): Packer[In, Unit] = new FromListPacker[In](from)


  def fork[In, Out](
    alternatives: List[Packer[In, Out]],
  ): Packer[In, Out] = new ForkPacker(alternatives)

  private class ForkPacker[In, Out](
    alternatives: List[Packer[In, Out]]
  ) extends Packer[In, Out] {
    override def take(input: List[In]): PackerResult[In, Out] = {
      println((input, "Fork for ", alternatives))
      alternatives.foldLeft(Failed(): PackerResult[In, Out]) {
        case (result @ Done(_, Nil), _) => result
        case (current @ Done(_, remaining), next) =>
          next.take(input) match {
            case betterResult @ Done(_, x) if x.size < remaining.size => betterResult
            case _ => current
          }
        case (_, next) =>
          next.take(input)
      }
    }
  }
  private class PurePacker[In, Out](value: Out) extends Packer[In, Out] {
    override def take(input: List[In]): PackerResult[In, Out] = {
      println(input -> "pure")
      Done(value, input)
    }
  }

  def flatMapResult[In, A, B](ta: Packer[In, A])
    (func: A => Packer[In, B]): Packer[In, B] = {
    new FlatMapPacker[In, A, B](ta, func)
  }

  private class FlatMapPacker[In, OutA, OutB](
    base: Packer[In, OutA],
    func: OutA => Packer[In, OutB]
  ) extends Packer[In, OutB] {
    override def take(input: List[In]): PackerResult[In, OutB] = {
      println(input -> "flatMap for " + base.toString)
      base.take(input) match {
        case Done(out0, remaining) =>
          val value = func(out0)
          value.take(remaining)
        case Failed() =>
          Failed()
      }
    }
  }

  def concat[In, Out1, Out2](
    t1: Packer[In, Out1],
    t2: => Packer[In, Out2]
  ): Packer[In, (Out1, Out2)] = flatMapResult(t1) {
    out1 =>
      mapResult(t2) {
        out2 => out1 -> out2
      }
  }

  def mapResult[In, Out0, Out1](t: Packer[In, Out0])
    (f: Out0 => Out1): Packer[In, Out1] = {
    input: List[In] => {
      println(input -> "map for" + t.toString)
      t.take(input) match {
        case Done(out, remaining) =>
          Done(f(out), remaining)
        case Failed() =>
          Failed()
      }
    }
  }
}
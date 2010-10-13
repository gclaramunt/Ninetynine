package exercises

object NinetyNine {
  //P01
  def last[A](xs: List[A]): A = xs match {
    case x :: Nil => x
    case x :: xs => last(xs)
    case Nil => throw new NoSuchElementException
  }

  //P02
  def penultimate[A](xs: List[A]): A = xs match {
    case x :: y :: Nil => x
    case x :: xs => penultimate(xs)
    case _ => throw new NoSuchElementException
  }

  //P03
  def nth[A](i: Int, xs: List[A]): A = xs match {
    case x :: xs => if (i == 0) x else nth(i - 1, xs)
    case _ => throw new NoSuchElementException
  }

  def nth2[A](i: Int, xs: List[A]): A = xs match {
    case x :: xs if (i == 0) => x
    case x :: xs if (i > 0) => nth(i - 1, xs)
    case _ => throw new NoSuchElementException
  }

  //P04
  def length[A](xs: List[A]): Int = xs match {
    case x :: xs => 1 + length(xs)
    case Nil => 0
  }

  //Although straight recursion is fun, using folds is more effective
  // folds are abstractions for recursion over data structures

  //P04
  def length1[A](xs: List[A]): Int = xs.foldLeft(0)((a, b) => a + 1)

  //P05
  def reverse[A](xs: List[A]): List[A] = xs.foldLeft(Nil: List[A])((a, b) => b :: a)

  //P06
  def isPalindrome[A](xs: List[A]): Boolean = reverse(xs) == xs

  //P07
  def flatten[A](xs: List[List[A]]): List[A] = xs.foldRight(Nil: List[A])(
    (xs, acc) => xs match {
      case ys: List[A] => ys ::: acc
      case y: A => y :: acc
    })

  //P08
  def compress[A](xs: List[A]): List[A] =
    xs.foldRight(List[A]())((y, ys) =>
      if (ys != Nil && ys.head == y) ys else y :: ys)

  //P09
  def pack[A](xs: List[A]): List[List[A]] = xs match {
    case Nil => Nil
    case _ => xs.takeWhile(_ == xs.head) :: pack(xs.dropWhile(_ == xs.head)) //can use span too, see P13
  }

  //P10
  def encode[A](xs: List[A]): List[(Int, A)] =
    pack(xs).map(y => (y.length, y.head))

  //P11
  def encodeModified[A](xs: List[A]): List[Any] =
    pack(xs).map(
      y => if (y.length == 1) y.head else (y.length, y.head))

  //Type safe version
  def encodeModifiedTypSafe[A](xs: List[A]): List[Either[A, (Int, A)]] =
    pack(xs).map(
      y =>
        if (y.length == 1) Left(y.head)
        else Right(y.length, y.head))

  //P12
  def decode[A](xs: List[(Int, A)]): List[A] = {
    def expand(i: Int, a: A): List[A] = if (i == 0) Nil else a :: expand(i - 1, a)
    flatten(xs.foldRight(List[List[A]]())((y, ys) => expand(y._1, y._2) :: ys))
  }
  //is better with flatMap
  def decode1[A](xs: List[(Int, A)]): List[A] =
    xs.flatMap(ny => List.make(ny._1, ny._2))

  //P13
  def encodeDirect[A](xs: List[A]): List[(Int, A)] = xs match {
    case Nil => Nil
    case _ => {
      val ys = xs.span(_ == xs.head)
      (ys._1.length, xs.head) :: encodeDirect(ys._2)
    }
  }

  //P14
  def duplicate[A](xs: List[A]): List[A] =
    xs.foldRight(Nil: List[A])((y, ys) => y :: y :: ys)
  //is better with flatMap
  def duplicate1[A](xs: List[A]): List[A] = xs.flatMap(y => y :: y :: Nil)

  //P15
  def duplicateN[A](n: Int, xs: List[A]): List[A] =
    xs.foldRight(Nil: List[A])((y, ys) => (1 to n).toList.map(_ => y) ::: ys)
  //is better with flatMap
  def duplicateN1[A](n: Int, xs: List[A]): List[A] =
    xs.flatMap(y => List.make(n, y))

  //P16
  def drop[A](n: Int, xs: List[A]): List[A] =
    xs.zipWithIndex.filter(_._2 % n != 0).map(_._1)

  //P17
  def split[A](n: Int, xs: List[A]): (List[A], List[A]) =
    xs.splitAt(n) // or (xs.take(n),xs.drop(n))

  //ok, that was too easy, let's try recursion
  def splitRec[A](n: Int, xs: List[A]): (List[A], List[A]) =
    (n, xs) match {
      case (0, _) => (Nil, xs)
      case (_, y :: ys) => {
        val rec = splitRec(n - 1, ys)
        (y :: rec._1, rec._2)
      }
      case (_, Nil) => (xs, Nil)
    }

  //P18
  def slice[A](s: Int, e: Int, xs: List[A]): List[A] =
    xs.slice(s, e) // or xs.drop(s).take(e-s)

  //with recursion
  def sliceRec[A](s: Int, e: Int, xs: List[A]): List[A] =
    (s, e, xs) match {
      case (0, 0, xs) => Nil
      case (0, _, y :: ys) => y :: sliceRec(0, e - 1, ys)
      case (_, _, y :: ys) => sliceRec(s - 1, e - 1, ys)
    }

  //P19
  def rotate[A](n: Int, xs: List[A]): List[A] = {
    val s = split((if (n > 0) n else n + xs.length), xs)
    s._2 ::: s._1
  }

  //P20
  def removeAt[A](n: Int, xs: List[A]): (List[A], A) = {
    val (heads, tails) = split(n, xs)
    (heads ::: tails.tail, tails.head)
  }

  //P21
  def insertAt[A](e: A, n: Int, xs: List[A]): List[A] = {
    val (heads, tails) = split(n, xs)
    heads ::: e :: tails
  }
  //with recursion
  def insertRecAt[A](e: A, n: Int, xs: List[A]): List[A] =
    (n, xs) match {
      case (0, ys) => e :: ys
      case (_, y :: ys) => y :: insertRecAt(e, n - 1, ys)
    }

  //P22
  def range[A](s: Int, e: Int): List[Int] = (s to e).toList

  //I don't think is the purpose of the exercise! Again, let's try recursion
  def rangeRec(s: Int, e: Int): List[Int] = if (e - s == 0) e :: Nil else s :: rangeRec(s + 1, e)

  //recursion and pattern matching with guards
  //a little more readable
  def rangeRecPm(s: Int, e: Int): List[Int] = (e - s) match {
    case x if (x > 0) => s :: rangeRecPm(s + 1, e)
    case x if (x == 0) => e :: Nil
    case _ => error("Invalid range")
  }

  //P23
  def randomSelect[A](n: Int, xs: List[A]): List[A] = (n, xs) match {
    case (_, Nil) => Nil
    case (0, _) => Nil
    case (_, _) => {
      val x = removeAt(new Random().nextInt(xs.size), xs)
      x._2 :: randomSelect(n - 1, x._1)
    }
  }

  //P24
  def lotto(n: Int, max: Int): List[Int] = randomSelect(n, (1 to max).toList)

  //P25
  def randomPermute[A](xs: List[A]): List[A] = randomSelect(xs.size, xs)

  //P26
  def combinations[A](n: Int, xs: List[A]): List[List[A]] = {
    def lift[A](xs: List[A]): List[List[A]] = xs.foldLeft(List[List[A]]())((ys, y) => (List(y) :: ys))

    (n, xs) match {
      case (1, ys) => lift(ys)
      case (i, xs) if (i == xs.size) => xs :: Nil
      case (i, ys) => combinations(i - 1, ys.tail).map(zs => ys.head :: zs) ::: combinations(i, ys.tail)
    }
  }
}
package com.hustlestar.scala_for_impatient.collections_13

import scala.collection.immutable

object CollectionsTasks {

  /**
    * Task 1:
    *
    * Write a function that, given a string, produces a map of the indexes of all characters.
    * For example, `indexes("Mississippi")` should return a map associating
    * 'M' with the set {0},
    * ‘i’ with the set {1, 4, 7, 10}, and so on.
    * Use a mutable map of characters to mutable sets. How can you ensure that the set is sorted?
    *
    * Solution:
    *
    * We have to use `LinkedHashSet` to maintain the indices order in set.
    */
  def indexes(s: String): Map[Char, immutable.IndexedSeq[Int]] = {
    s.zipWithIndex
      .groupBy(_._1)
      .mapValues(_.map(_._2))
  }

  /**
    * Task 3:
    *
    * Write a function that removes all zeroes from a linked list of integers.
    */
  def deleteZeros(l: List[Int]): List[Any] = {
    l.filterNot(_ == 0)
  }

  /**
    * Task 4:
    *
    * Write a function that receives a collection of strings and a map from strings to integers.
    * Return a collection of integers that are values of the map corresponding to one of
    * the strings in the collection. For example, given
    * `Array("Tom", "Fred", "Harry")` and `Map("Tom" -> 3, "Dick" -> 4, "Harry" -> 5)`,
    * return `Array(3, 5)`. Hint: Use flatMap to combine the Option values returned by get.
    */
  def mapArrayToMap(strings: Array[String], stringToInt: Map[String, Int]): Array[Int] = {
    strings.flatMap(stringToInt.get)
  }

  /**
    * Task 5:
    *
    * Implement a function that works just like `mkString`, using `reduceLeft`.
    */
  def collMkString[String](col: Traversable[String], sep: String): Predef.String = {
    //xs.reduceLeft(_ + _)
    col.map(a => a.toString)
      .reduceLeft(_ + sep + _)
  }

  /**
    * Task 6:
    *
    * Given a list of integers `lst`, what is
    * `(lst :\ List[Int]())(_ :: _)` ?
    * `(List[Int]() /: lst)(_ :+ _)` ?
    * How can you modify one of them to reverse the list?
    *
    * Solution:
    *
    * The first expression executes `foldRight` and prepends the elements to the resulting list.
    * The second expression executes `foldLeft` and appends the elements to the resulting list.
    * Both expressions produce new `List[Int]` with the same elements as in the original list,
    * and in the same order.
    * To reverse the list its better to modify the second expression to prepend the elements,
    * which is cheaper for lists comparing to append.
    */
  def reverseList(lst: List[Int]): List[Int] = {
    (List[Int]() /: lst) ((t, h) => h :: t)
  }

  /**
    * Task 7:
    *
    * In Section 13.11, "Zipping", on page 171, the expression
    * {{{
    *  (prices zip quantities) map { p => p._1 * p._2 }
    * }}}
    * is a bit inelegant. We can't do
    * {{{
    *  (prices zip quantities) map { _ * _ }
    * }}}
    * because `_ * _` is a function with two arguments, and we need a function with one argument
    * that is a tuple. The `tupled` method of the `Function` object changes a function with
    * two arguments to one that take a tuple. Apply `tupled` to the multiplication function
    * so you can map it over the list of pairs.
    */
  def zipAndMultiply(prices: Iterable[Int], quantities: Iterable[Int]): Iterable[Int] = {
    val zipped: Iterable[(Int, Int)] = prices.zip(quantities)
    zipped.map(Function.tupled(_ * _))
  }

  /**
    * Task 8:
    *
    * Write a function that turns an array of `Double` values into a two-dimensional array.
    * Pass the number of columns as a parameter. For example, with `Array(1, 2, 3, 4, 5, 6)`
    * and three columns, return `Array(Array(1, 2, 3), Array(4, 5, 6))`. Use the `grouped` method.
    */
  def twoDimensionalArray(arr: Array[Double], columns: Int): Array[Array[Double]] = {
    arr.grouped(columns)
      .toArray
  }

  /**
    * Task 9:
    *
    * Harry Hacker writes a program that accepts a sequence of file names on the command line.
    * For each, he starts a new thread that reads the file and updates a letter frequency map
    * declared as
    * {{{
    *  val frequencies = new scala.collection.mutable.HashMap[Char, Int] with
    *    scala.collection.mutable.SynchronizedMap[Char, Int]
    * }}}
    * When reading a letter `c`, he calls
    * {{{
    *  frequencies(c) = frequencies.getOrElse (c, 0) + 1
    * }}}
    * Why won't this work? Will it work if he used instead
    * {{{
    *  import scala.collection.JavaConversions.asScalaConcurrentMap
    *  val frequencies: scala.collection.mutable.ConcurrentMap[Char, Int] =
    *    new java.util.concurrent.ConcurrentHashMap[Char, Int]
    * }}}
    *
    * Solution:
    *
    * It won't work with SynchronizedMap since its not synchronize addition operation.
    * And its not enough using ConcurrentHashMap, we also need to perform threadsafe addition.
    * See the fixed code below.
    */
  def createLetterFrequencyMap(files: Iterable[String]): Map[Char, Int] = {
    import scala.collection.JavaConversions.mapAsScalaConcurrentMap
    val res = new java.util.concurrent.ConcurrentHashMap[Char, Int]
    val threads = files.map(file => new Thread(new Runnable {
      override def run(): Unit = {
        println(Thread.currentThread().getName + " " + file)
        file.toCharArray
          .foreach {
            c => {
              val x = res.getOrElse(c, 0)
              res.update(c, x + 1)
            }
          }
      }
    }))
    threads.foreach(_.start)
    threads.foreach(_.join)
    res.toMap
  }
}
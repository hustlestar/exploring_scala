package chapter_13_collections

import com.hustlestar.scala.`for`.impatient.collections_13.CollectionsTasks
import org.scalatest.{FlatSpec, Matchers}

class CollectionsTasksSpec extends FlatSpec with Matchers {

  behavior of "An empty Set"

  it should "map chars to list of indexes in string" in {
    println(CollectionsTasks.indexes("Mississippi"))
    CollectionsTasks.indexes("Mississippi") shouldBe Map('M' -> Vector(0), 's' -> Vector(2, 3, 5, 6), 'p' -> Vector(8, 9), 'i' -> Vector(1, 4, 7, 10))
  }

  it should "delete nulls from list" in {
    println(CollectionsTasks.deleteZeros(List(1, 2, 3, 4, 0, 5, 6, 7, 0)))
    CollectionsTasks.deleteZeros(List(1, 2, 3, 4, 0, 5, 6, 7, 0)) shouldBe List(1, 2, 3, 4, 5, 6, 7)
  }

  it should "map list to map" in {
    val res = CollectionsTasks.mapArrayToMap(Array("Tom", "Fred", "Harry"), Map("Tom" -> 3, "Dick" -> 4, "Harry" -> 5))
    println(res)
    res shouldBe Array(3, 5)
  }

  it should "be same as mkString" in {
    val list = List(1, 2, 3, 4)
    val sep = " "
    val res = CollectionsTasks.collMkString(list, sep)
    println(res)
    val expected = list.mkString(sep)
    res shouldBe expected
  }

  it should "reverse list" in {
    val list = List(1, 2, 3, 4)
    val res = CollectionsTasks.reverseList(list)
    println(res)
    res shouldBe list.reverse
  }

  it should "zip 2 lists and multiplies k and v" in {
    val res = CollectionsTasks.zipAndMultiply(List(10, 20, 30, 40, 50), List(3, 1, 4, 5))
    println(res)
    res shouldBe List(30, 20, 120, 200)
  }
}

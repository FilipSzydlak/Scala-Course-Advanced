package exercises

import scala.annotation.tailrec

trait MySet[A] extends (A => Boolean){

  /*
    EXERCISE - implement a functional set
  */
  def apply(elem: A): Boolean =
    contains(elem)

  def contains(elem: A): Boolean
  def +(elem: A): MySet[A]
  def ++(anotherSet: MySet[A]): MySet[A] // union

  def map[B](f: A => B): MySet[B]
  def flatMap[B](f: A => MySet[B]): MySet[B]
  def filter(predicate: A => Boolean): MySet[A]
  def foreach(f: A => Unit): Unit

  /*
  - removing an element
  - intersection with another set
  - difference with another set
  */
  def -(elem: A): MySet[A]
  def --(anotherSet: MySet[A]): MySet[A] // difference
  def &(anotherSet: MySet[A]): MySet[A] // intersection

  def unary_! : MySet[A]
}

class EmptySet[A] extends MySet[A]{
  def contains(elem: A): Boolean = false
  def +(elem: A): MySet[A] = new NonEmptySet[A](elem, this)
  def ++(anotherSet: MySet[A]): MySet[A] = anotherSet

  def map[B](f: A => B): MySet[B] = new EmptySet[B]
  def flatMap[B](f: A => MySet[B]): MySet[B] = new EmptySet[B]
  def filter(predicate: A => Boolean): MySet[A] = this
  def foreach(f: A => Unit): Unit = ()

  // part2
  def -(elem: A): MySet[A] = this
  def --(anotherSet: MySet[A]): MySet[A] = this // difference
  def &(anotherSet: MySet[A]): MySet[A] = this // intersection

  def unary_! : MySet[A] = new AllInclusiveSet[A]


}

class PropertyBasedSet[A](property:) extends MySet[A]{
  override def apply(elem: A): Boolean = true

  override def +(elem: A): MySet[A] = this

  override def ++(anotherSet: MySet[A]): MySet[A]= this


  // naturals = allinclusiveSet[Int] = all the natural numbers
  // naturals.map(x => x % 3) => ???
  // [0 1 2]
  def unary_! : MySet[A] = new AllInclusiveSet[A]
}

// all elements of type A which satisfy a property
// { x in A | property (x) }
class PropertyBasedSet[A](property: A => Boolean) extends MySet[A]{
  def contains(elem: A): Boolean = property(elem)

  // { x in A | property(x) } + element = { x in A | property(x) || x == element}
  def + (elem: A): MySet[A] =
    new PropertyBasedSet[A](x => property(x) || x == elem)

  // { x in A | property(x) } ++ set =>  { x in A | property(x)  || set contains x}
  def ++(anotherSet: MySet[A]): MySet[A] =
    new PropertyBasedSet[A](x => property(x) || anotherSet(x))




}

class NonEmptySet[A](head: A, tail: MySet[A]) extends MySet[A]{
  def contains(elem: A): Boolean =
    elem == head || tail.contains(elem)

  def +(elem: A): MySet[A] =
    if (this contains elem) this
    else  new NonEmptySet[A](elem, this)

  /*
    [1 2 3] ++ [4 5] =
    [2 3] ++ [4 5] + 1 =
    [3] ++ [4 5] + 1 + 2 =
    [] ++ [4 5] + 1 + 2 + 3
    [4 5] + 1 + 2 + 3 = [4 5 1 2 3]
   */
  def ++(anotherSet: MySet[A]): MySet[A] =
    tail ++ anotherSet + head

  def map[B](f: A => B): MySet[B] = (tail map f) + f(head)
  def flatMap[B](f: A => MySet[B]): MySet[B] = (tail flatMap f) ++ f(head)
  def filter(predicate: A => Boolean): MySet[A] = {
    val filteredTail = tail filter predicate
    if (predicate(head)) filteredTail + head
    else filteredTail
  }
  def foreach(f: A => Unit): Unit = {
    f(head)
    tail foreach f
  }

  // part 2

  def -(elem: A): MySet[A] =
    if(head == elem) tail
    else tail - elem + head

  def --(anotherSet: MySet[A]): MySet[A] =  filter(x => !anotherSet(x))// difference
  def &(anotherSet: MySet[A]): MySet[A] = filter(anotherSet) // intersection = filtering

  // new operator
  def unary_! : MySet[A]
}

object MySet {
  def apply[A](values: A*): MySet[A] = {
    @tailrec
    def buildSet(valSeq: Seq[A], acc: MySet[A]): MySet[A] =
      if(valSeq.isEmpty) acc
      else buildSet(valSeq.tail, acc + valSeq.head)

    buildSet(values.toSeq, new EmptySet[A])
  }
}

object MySetPlayground extends App {
  val s = MySet(1,2,3,4)
  s + 5 ++ MySet(-1,-2) + 3 flatMap (x=> MySet(x, 10 * x)) filter (_ % 2 ==0) foreach println
}

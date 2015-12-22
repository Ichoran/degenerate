// This file is distributed under the Apache 2 license.  See file LICENSE.
// Copyright (c) 2015 Rex Kerr and Calico Labs.

package degen

import collection.mutable.{AnyRefMap => RMap}

trait Interpretation[A] {
  def hash(a: A): Int
  def str(a: A): String
  def eqto(a: A, o: Any): Boolean  
}

trait Grammer[A] extends Interpretation[A] {
  def len(a: A): Int
  def cut(a: A, i: Int, n: Int): A
}
object Grammer {
  implicit val stringGrammer = new Grammer[String] {
    def len(a: String) = a.length
    def cut(a: String, i: Int, n: Int) = a.substring(i, i+n)
    def hash(a: String) = scala.util.hashing.MurmurHash3.stringHash(a)
    def str(a: String) = a
    def eqto(a: String, o: Any) = a == o
  }
  implicit val arrayIntGrammer = new Grammer[Array[Int]] {
    def len(a: Array[Int]) = a.length
    def cut(a: Array[Int], i: Int, n: Int) = java.util.Arrays.copyOfRange(a, i, i+n)
    def hash(a: Array[Int]) = {
      var h = scala.util.hashing.MurmurHash3.arraySeed
      var i = 0
      while (i+1 < a.length) {
        h = scala.util.hashing.MurmurHash3.mix(h, a(i))
        i += 1
      }
      if (i < a.length) h = scala.util.hashing.MurmurHash3.mixLast(h, a(i))
      scala.util.hashing.MurmurHash3.finalizeHash(h, a.length)
    }
    def str(a: Array[Int]) = a.mkString(", ")
    def eqto(a: Array[Int], o: Any): Boolean = o match {
      case b: Array[Int] =>
        a.length == b.length && {
          var i = 0
          while (i < a.length) { if (a(i) != b(i)) return false; i += 1 }
          true
        }
      case _ => false
    }
  }
}

class CachedHashed[A](val value: A)(implicit interp: Interpretation[A]) {
  def interpretation = interp
  override lazy val hashCode = interp.hash(value)
  override lazy val toString = interp.str(value)
  override def equals(a: Any) = value match {
    case ch: CachedHashed[_] => interp.eqto(value, ch.value)
    case _                   => interp.eqto(value, a)
  }
}

class NGram[A](val text: A) {
  private[this] var mySize = 0
  private[this] var isSorted = true
  private[this] var myIndices = new Array[Int](6)
  private[this] def checkSize(): this.type = {
    if (mySize >= myIndices.length) myIndices = java.util.Arrays.copyOf(myIndices, ((myIndices.length << 1) | 2) & 0x7FFFFFFE)
    this
  }
  def size = mySize
  def apply(i: Int) = {
    if (!isSorted) { isSorted = true; java.util.Arrays.sort(myIndices, 0, mySize) }
    myIndices(i)
  }
  def export(target: Array[Int], i0: Int) {
    if (!isSorted) { isSorted = true; java.util.Arrays.sort(myIndices, 0, mySize) }
    System.arraycopy(myIndices, 0, target, i0, mySize)
  }
  def +=(x: Int) {
    checkSize()
    myIndices(mySize) = x
    if (isSorted && mySize > 0 && myIndices(mySize-1) > x) isSorted = false
    mySize += 1
  }
}
object NGram {
  def apply[A: Grammer]: RMap[CachedHashed[A], NGram[A]] = ???
}

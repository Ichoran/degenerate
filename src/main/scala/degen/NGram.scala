// This file is distributed under the Apache 2 license.  See file LICENSE.
// Copyright (c) 2015 Rex Kerr and Calico Labs.

package degen

import collection.mutable.{AnyRefMap => RMap}

trait Interpretation[A] {
  def hash(a: A): Int
  def str(a: A): String
  def eqto(a: A, o: Any): Boolean  
}
object Interpretation {
  implicit def searchGrammers[A](implicit gm: Grammer[A]): Interpretation[A] = gm
}

trait Grammer[A] extends Interpretation[A] {
  def len(a: A): Int
  def cut(a: A, i: Int, n: Int): A
  def numeq(a: A, i: Int, aa: A, j: Int): Int
}
object Grammer {
  implicit val stringGrammer = new Grammer[String] {
    def len(a: String) = a.length
    def cut(a: String, i: Int, n: Int) = a.substring(i, i+n)
    def hash(a: String) = scala.util.hashing.MurmurHash3.stringHash(a)
    def str(a: String) = a
    def eqto(a: String, o: Any) = a == o
    def numeq(a: String, i: Int, aa: String, j: Int): Int = {
      var m = i
      var n = j
      while (m < a.length && n < aa.length && a.charAt(m) == aa.charAt(n)) { m += 1; n += 1 }
      m - i
    }
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
    def numeq(a: Array[Int], i: Int, aa: Array[Int], j: Int): Int = {
      var m = i
      var n = j
      while (m < a.length && n < aa.length && a(m) == aa(n)) { m += 1; n += 1 }
      m - i
    }
  }
}

class CachedHashed[A](val value: A)(implicit interp: Interpretation[A]) {
  def interpretation = interp
  override lazy val hashCode = interp.hash(value)
  override lazy val toString = interp.str(value)
  override def equals(a: Any) = a match {
    case ch: CachedHashed[_] => interp.eqto(value, ch.value)
    case _                   => interp.eqto(value, a)
  }
  def cached: this.type = this
}
object CachedHashed {
  implicit def cacheIfInterpretable[A](value: A)(implicit interp: Interpretation[A]) = new CachedHashed[A](value)
}

class NGram[A: Interpretation](val text: A) {
  def interpretation = implicitly[Interpretation[A]]
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
  def toIndexArray: Array[Int] = { val a = new Array[Int](size); export(a, 0); a }
  def +=(x: Int): this.type = {
    checkSize()
    myIndices(mySize) = x
    if (isSorted && mySize > 0 && myIndices(mySize-1) > x) isSorted = false
    mySize += 1
    this
  }
  override def toString = myIndices.iterator.take(mySize).mkString("'"+interpretation.str(text)+"'{", ",","}")
}
object NGram {
  import CachedHashed._
  def apply[A: Grammer](a: A): RMap[CachedHashed[A], NGram[A]] = {
    val m = new RMap[CachedHashed[A], NGram[A]]
    val temp = new collection.mutable.ArrayBuffer[CachedHashed[A]]
    val gm = implicitly[Grammer[A]]
    var i = 0
    val L = gm.len(a)
    while (i < L) {
      val ai = gm.cut(a, i, 1)
      m.getOrElseUpdate(ai.cached, new NGram[A](ai)) += i
      i += 1
    }
    m.foreach{ case (k, ng) => if (ng.size <= 1) temp += k }
    temp.foreach{ k => m -= k }
    temp.clear
    var source: Array[CachedHashed[A]] = m.keys.toArray
    while (source.nonEmpty) {
      source.foreach{ k =>
        val d = gm.len(k.value)
        m(k).toIndexArray.
          collect{ case j if j+d < L => (gm.cut(a, j+d, 1).cached, j+d) }.
          groupBy(_._1).foreach{ case (_, vs) =>
            if (vs.length > 1) {
              vs.groupBy(v => gm.numeq(a, v._2, a, if (v._2 == vs(0)._2) vs(1)._2 else vs(0)._2)).foreach{ case (n, us) => 
                if (us.length > 1) {
                  val kk = gm.cut(a, us(0)._2 - d, d + n).cached
                  temp += kk
                  val x = new NGram[A](kk.value)
                  var ui = 0
                  while (ui < us.length) { x += (us(ui)._2 - d); ui += 1 }
                  m += (kk, x)
                }
              }
            }
          }
      }
      source = temp.toArray
      temp.clear
    }
    m
  }
}

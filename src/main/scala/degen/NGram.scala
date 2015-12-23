// This file is distributed under the Apache 2 license.  See file LICENSE.
// Copyright (c) 2015 Rex Kerr and Calico Labs.

package degen

import collection.mutable.{AnyRefMap => RMap}

class StringBit(val source: String, val i0: Int, val iN: Int) {}
class ArrayIntBit(val source: Array[Int], val i0: Int, val iN: Int) {}

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
  def extra(a: A, b: A): Int
}
object Grammer {
  implicit val substringGrammer = new Grammer[StringBit] {
    def len(a: StringBit) = a.iN - a.i0
    def cut(a: StringBit, i: Int, n: Int) = {
      val j = math.max(0, math.min(a.i0.toLong + i, a.source.length))
      val m = math.min(j+n, a.source.length)
      new StringBit(a.source, j.toInt, m.toInt)
    }
    def hash(a: StringBit) = {
      var h = scala.util.hashing.MurmurHash3.stringSeed
      var i = a.i0
      while (i < a.iN-1) {
        h = scala.util.hashing.MurmurHash3.mix(h, (a.source.charAt(i) << 16) + a.source.charAt(i+1))
        i += 2
      }
      if (i < a.iN) h = scala.util.hashing.MurmurHash3.mixLast(h, a.source.charAt(i))
      scala.util.hashing.MurmurHash3.finalizeHash(h, a.iN - a.i0)
    }
    def str(a: StringBit) = a.source.substring(a.i0, a.iN)
    def eqto(a: StringBit, o: Any): Boolean = o match {
      case b: StringBit =>
        if (len(a) != len(b)) false
        else if ((a.source eq b.source) && a.i0 == b.i0 && a.iN == b.iN) true
        else {
          var i = a.i0
          var j = b.i0
          val as = a.source
          val bs = b.source
          while (i < a.iN) {
            if (as.charAt(i) != bs.charAt(j)) return false
            i += 1
            j += 1
          }
          true
        }
      case s: String =>
        if (s.length != len(a)) false
        else {
          var i = a.i0
          var j = 0
          while (j < s.length) {
            if (a.source.charAt(i) != s.charAt(j)) return false
            i += 1
            j += 1
          }
          true
        }
      case _ => false
    }
    def extra(a: StringBit, b: StringBit): Int = {
      if ((a.source eq b.source) && a.iN == b.iN) a.source.length - a.iN
      else {
        var i = a.iN
        var j = b.iN
        while (i < a.source.length && j < b.source.length && a.source.charAt(i) == b.source.charAt(j)) { i += 1; j += 1 }
        i - a.iN
      }
    }
  }
  implicit val subarrayIntGrammer = new Grammer[ArrayIntBit] {
    def len(a: ArrayIntBit) = a.iN - a.i0
    def cut(a: ArrayIntBit, i: Int, n: Int) = {
      val j = math.max(0, math.min(a.i0.toLong + i, a.source.length))
      val m = math.min(j+n, a.source.length)
      new ArrayIntBit(a.source, j.toInt, m.toInt)
    }
    def hash(a: ArrayIntBit) = {
      var h = scala.util.hashing.MurmurHash3.arraySeed
      var i = a.i0
      while (i < a.iN-1) {
        h = scala.util.hashing.MurmurHash3.mix(h, a.source(i))
        i += 1
      }
      if (i < a.iN) h = scala.util.hashing.MurmurHash3.mixLast(h, a.source(i))
      scala.util.hashing.MurmurHash3.finalizeHash(h, len(a))
    }
    def str(a: ArrayIntBit) = {
      val sb = new StringBuilder
      sb += '{'
      var i = a.i0
      while (i < a.iN) { 
        if (i > a.i0) sb ++= ", "
        sb ++= a.source(i).toString
      }
      sb += '}'
      sb.result()
    }
    def eqto(a: ArrayIntBit, o: Any): Boolean = o match {
      case b: ArrayIntBit =>
        len(a) == len(b) && {
          var i = a.i0
          var j = b.i0
          while (i < a.iN) { if (a.source(i) != b.source(j)) return false; i += 1; j += 1 }
          true
        }
      case b: Array[Int] =>
        len(a) == b.length && {
          var i = a.i0
          var j = 0
          while (j < b.length) { if (a.source(i) != b(j)) return false; i += 1; j += 1 }
          true
        }
      case _ => false
    }
    def extra(a: ArrayIntBit, b: ArrayIntBit): Int = {
      if ((a.source eq b.source) && a.iN == b.iN) a.source.length - a.iN
      else {
        var i = a.iN
        var j = b.iN
        while (i < a.source.length && j < b.source.length && a.source(i) == b.source(j)) { i += 1; j += 1 }
        i - a.iN        
      }
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
    if (mySize >= myIndices.length) {
      var L = myIndices.length
      while ((mySize & 0x7FFFFFFE) >= L) L = ((L << 1) | 2) & 0x7FFFFFFE
      myIndices = java.util.Arrays.copyOf(myIndices, L)
    }
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
  def ++=(source: Array[Int], i0: Int, iN: Int): this.type = {
    var j = mySize
    mySize += (iN - i0) - 1
    checkSize()
    var i = i0
    while (i < iN) {
      myIndices(j) = source(i)
      if (isSorted && j > 0 && myIndices(j-1) > myIndices(j)) isSorted = false
      i += 1
      j += 1
    }
    mySize += 1
    this
  }
  def ++=(source: Array[Int]): this.type = this ++= (source, 0, source.length)
  override def toString = myIndices.iterator.take(mySize).mkString("'"+interpretation.str(text)+"'{", ",","}")
}
object NGram {
  import CachedHashed._
  def apply[A: Grammer](a: A): RMap[CachedHashed[A], NGram[A]] = {
    val m, minim = new RMap[CachedHashed[A], NGram[A]]
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
    val b, c = gm.cut(a, 0, 0)
    while (source.nonEmpty) {
      source.foreach{ k =>
        val l = gm.len(k.value)
        val ixs = m(k).toIndexArray
        var least = Int.MaxValue
        var j = 1
        while (j < ixs.length && least > 0) {
          val n = gm.extra(k.value, gm.cut(a, ixs(j), l))
          least = math.min(least, n)
          j += 1
        }
        if (least > 0) {
          val kk = gm.cut(a, ixs(0), l + least).cached
          m -= k
          m += (kk, (new NGram(kk.value)) ++= ixs)
          temp += kk
        }
        else {
          ixs.groupBy(i => gm.cut(a, i+l, 1).cached).foreach{ case (_, iys) =>
            if (iys.length > 1) {
              val kk = gm.cut(a, iys(0), l+1).cached
              m += (kk, (new NGram(kk.value)) ++= iys)
              temp += kk
            }
          }
        }
      }
      source = temp.toArray
      temp.clear
    }
    m.foreach{ case (k,ng) =>
      val l = gm.len(k.value)
      if (l > 1 && ng(0) > 0) {
        val probe = gm.cut(a, ng(0)-1, l+1).cached
        m.get(probe) match {
          case Some(xg) =>
            if (ng.size == xg.size && (0 until ng.size).forall(i => ng(i) == xg(i)+1)) {
              temp += k
            }
          case _ =>
        }
      }
    }
    temp.foreach{ k => m -= k }
    m
  }
}

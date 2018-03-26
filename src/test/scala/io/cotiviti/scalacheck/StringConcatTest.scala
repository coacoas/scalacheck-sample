/**
  * Copyright (C) 2018 Cotiviti Labs (nexgen.admin@cotiviti.io)
  *
  * The software code contained herein is the property of Cotiviti Corporation
  * and its subsidiaries and affiliates (collectively, “Cotiviti”).
  * Access to this software code is being provided to you in the course of your
  * employment or affiliation with Cotiviti and may be used solely in the scope
  * and course of your work for Cotiviti, and is for internal Cotiviti use only.
  * Any unauthorized use, disclosure, copying, distribution, destruction of this
  * software code, or the taking of any unauthorized action in reliance on this
  * software code, is strictly prohibited.
  * If this information is viewed in error, immediately discontinue use of the
  * application.  Anyone using this software code and the applications will be
  * subject to monitoring for improper use, system maintenance and security
  * purposes, and is advised that if such monitoring reveals possible criminal
  * activity or policy violation, Cotiviti personnel may provide the evidence of
  * such monitoring to law enforcement or other officials, and the user may be
  * subject to disciplinary action by Cotiviti, up to and including termination
  * of employment.
  *
  * Use of this software code and any applications and information therein
  * constitutes acknowledgement of and consent to this notice
  */
package io.cotiviti.scalacheck

import org.scalacheck.{ Arbitrary, Gen, Shrink }
import org.scalatest.{ FlatSpec, Matchers }
import org.scalatest.prop.PropertyChecks


class StringConcatTest extends FlatSpec
    with PropertyChecks
    with Matchers
{
  import StringConcatTest._

  "A concatenated string" should
  "have length equal to the sum of the lengths of the two strings" in
  forAll { (s1: String, s2: String) =>
    val s = s1 + s2
    s.length should equal(s1.length + s2.length)
  }

  it should "start with the first string" in forAll { (s1: String, s2: String) =>
    (s1 + s2) should startWith(s1)
  }

  it should "end with the second string" in forAll { (s1: String, s2: String) =>
    (s1 + s2) should endWith(s2)
  }

  "a NonEmptyString" should "have length > 0" in forAll { ne: NonEmptyString =>
    ne.value.length should be > 0
  }
}
















































object StringConcatTest {
  case class NonEmptyString(val value: String) extends AnyVal

  val nonEmptyStringGenerator: Gen[NonEmptyString] = for {
    c <- Gen.alphaNumChar
    s <- Gen.alphaNumStr
  } yield NonEmptyString(c.toString + s)

  implicit val arbitraryNonEmptyString: Arbitrary[NonEmptyString] =
    Arbitrary(nonEmptyStringGenerator)

  implicit val nonEmptyStringShrink: Shrink[NonEmptyString] = Shrink {
    (s: NonEmptyString) =>
    val l = s.value.length
    if (l == 1) {
      Stream.empty
    } else {
      Stream(NonEmptyString(s.value.substring(l / 2)))
    }
  }
}

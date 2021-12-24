package karazin.scala.users.group.week2.homework

import scala.math._
import org.scalacheck._
import Prop.{forAll, propBoolean, throws}
import karazin.scala.users.group.week2.homework.arbitraries
import Homework._
import utils._

object HomeworkSpecification extends Properties("Homework"):
  import arbitraries.{given Arbitrary[Int], given Arbitrary[Rational]}

  property("throw exception due to zero denominator") = forAll { (numer: Int) ⇒
    throws(classOf[IllegalArgumentException]) {
      Rational(numer, 0)
    }
  }

  property("throw exception due to negative denominator") = forAll { (numer: Int, kindaDenom: Int) ⇒
    throws(classOf[IllegalArgumentException]) {
      Rational(numer, -abs(kindaDenom))
    }
  }

  property("check that rational number is simplified") = forAll { (numer: Int, int: Int) ⇒
    val denom = abs(int) + 1c
    val rational = Rational(numer, denom)

    rational.numer == (numer / gcd(abs(numer), denom)) && rational.denom == (denom / gcd(abs(numer), denom))
  }

  property("check equals") = forAll { (left: Rational, right: Rational) ⇒
    (left == right) == (left.numer == right.numer && left.denom == right.denom)
  }

  property("less then") = forAll { (left: Rational, right: Rational) =>
    (left < right) == (left.numer * right.denom < right.numer * left.denom)
  }

  property("less or equal") = forAll { (left: Rational, right: Rational) =>
    (left <= right) == ( left < right || left == right)
  }

  property("greater") = forAll { (left: Rational, right: Rational) =>
    (left > right) == !(left <= right)
  }

  property("greater or equal") = forAll { (left: Rational, right: Rational) =>
    (left >= right) == ( left > right || left == right)
  }

  property("negation") = forAll { (rational: Rational) =>
    -(rational) == Rational(-rational.numer, rational.denom)
  }

  property("addition") = forAll { (left: Rational, right: Rational) =>
    (left + right) == (Rational(this.x * this.y * that.y / gcd(this.y, that.y) / this.y + that.x * this.y * that.y / gcd(this.y, that.y) / that.y, this.y * that.y / gcd(this.y, that.y));)
  }

  property("subtraction") = forAll { (left: Rational, right: Rational) =>
    (left - right) == Rational(this.x * this.y * that.y / gcd(this.y, that.y) / this.y - that.x * this.y * that.y / gcd(this.y, that.y) / that.y, this.y * that.y / gcd(this.y, that.y));
  }

  property("multiplication") = forAll { (left: Rational, right: Rational) =>
    (left * right) == Rational(this.numer * that.numer, this.denom * that.denom)
  }

  property("division") = forAll { (left: Rational, numer: Int, denom: Int) =>
    val right = Rational(if numer == 0 then 1 else numer, abs(denom) + 1)
    (left / right) == Rational(this.numer * that.denom, this.denom * that.numer)
  }

  property("division by zero") = forAll { (left: Rational, int: Int) =>
    throws(classOf[IllegalArgumentException]) {
      int == 0
    }
  }

end HomeworkSpecification
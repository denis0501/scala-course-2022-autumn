package karazin.scala.users.group.week1.homework

import org.scalacheck._
import Prop.{forAll, propBoolean}
import Homework._
import karazin.scala.users.group.week1.homework.arbitraries
import scala.annotation.tailrec

import `Look-and-say Sequence`._
import arbitraries.given Arbitrary[Int]

object HomeworkSpecification extends Properties("Homework") :

    include(BooleanOperatorsSpecification)
    include(FermatNumbersSpecification)
    include(LookAndSaySequenceSpecification)

end HomeworkSpecification

object BooleanOperatorsSpecification extends Properties("Boolean Operators") :

    import `Boolean Operators`._

    property("not") = forAll { (b: Boolean) =>
        not(b) == !b
    }

    property("and") = forAll { (pair: (Boolean, Boolean)) =>
        val (left, right) = pair

        and(left, right) == (left && right)
    }

    property("and with Exception") = propBoolean {
        !and(false, throw new Exception())
    }

    property("or") = forAll { (pair: (Boolean, Boolean)) =>
        val (left, right) = pair

        or(left, right) == (left || right)
    }

    property("or with Exception") = propBoolean {
        or(true, throw new Exception())
    }

end BooleanOperatorsSpecification

object FermatNumbersSpecification extends Properties("Fermat Numbers") :

    import `Fermat Numbers`._
    import arbitraries.given Arbitrary[Int]

    property("multiplication") = forAll { (left: Int, right: Int) =>
        multiplication(left, right) == (left * right)
    }

    property("power") = forAll { (left: Int, right: Int) =>
        power(left, right) == (0 until right).foldLeft(BigInt(1)) { (acc, _) => acc * left }
    }

    property("fermatNumber") = forAll { (n: Int) =>
        fermatNumber(n) == BigInt(2).pow(Math.pow(2, n).intValue()) + 1
    }

end FermatNumbersSpecification

object LookAndSaySequenceSpecification extends Properties("Look-and-say Sequence") :


    val modelLookAndSaySequence: (Int) => BigInt = (n: Int) => {
        val regex = """(\d)(\1*)""".r

        def lookAndSay(idx: Int, acc: String): String = {
            if (idx == n) acc
            else lookAndSay(
                idx + 1,
                regex
                  .findAllMatchIn(acc)
                  .map(m => m.group(0).length.toString + m.group(1)).mkString)
        }

        BigInt(lookAndSay(0, "1"))
    }

    property("LookAndSaySequence") = forAll { (n: Int) =>
        lookAndSaySequenceElement(n) == modelLookAndSaySequence(n)
    }

end LookAndSaySequenceSpecification
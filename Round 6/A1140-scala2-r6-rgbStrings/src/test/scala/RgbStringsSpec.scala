// Scala 2 (DO NOT EDIT OR REMOVE THIS LINE!!!)

/* Author: Tommi Junttila, Aalto University.
 * Only for the use on the Aalto course CS-A1140.
 * Redistribution not allowed.
 */

package tests

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

import rgbStrings._

class RgbStringsSpec extends AnyFlatSpec with Matchers {
  "The rgbStrings.count method" should "work correctly" in {
    count(1) should be (BigInt(3))
    count(2) should be (BigInt(7))
    count(3) should be (BigInt(17))
    count(10) should be (BigInt(8119))
    count(100) should be (BigInt("228725309250740208744750893347264645481"))
    count(1000) should be (BigInt("72016336943533875056131468444247239328723197628440751797201898063588088312700201943482948477109536203740206649612702729920170001354454107173480483962605519493117789821758457767858986227019805650639002566946496865364666543562826303377877700877266135276209125278037204443042487153089226849544681245300260167141025277156482737568934079466850318276966893735585103845471745828701580706481"))
  }
}

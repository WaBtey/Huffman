package fr.istic.si2.test.huffman

import org.junit.Test
import org.junit.Assert._
import fr.istic.si2.huffman.Encodage._
import fr.istic.si2.huffman.Decodage._
import fr.istic.si2.testerApp._
import fr.istic.si2.huffman._

class TestsUtils {

  val _ = new AppInit(HuffmanApp0) // Ne pas supprimer cette ligne.

  /**
   * Test de conversion d'une liste de bits en une string
   */
  @Test
  def testListBitToString () {
    assertEquals("", Utils.listBitToString(Nil))
    assertEquals("1", Utils.listBitToString(One::Nil))
    assertEquals("0", Utils.listBitToString(Zero::Nil))
    assertEquals("1010", Utils.listBitToString(One::Zero::One::Zero::Nil))
    assertEquals("0010", Utils.listBitToString(Zero::Zero::One::Zero::Nil))
  }

}

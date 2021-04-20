package fr.istic.si2.test.huffman

import org.junit.Test
import org.junit.Assert._
import fr.istic.si2.huffman.Encodage._
import fr.istic.si2.huffman.Decodage._
import fr.istic.si2.testerApp._
import fr.istic.si2.huffman._

class TestsDecodage {

  val _ = new AppInit(HuffmanApp0) // Ne pas supprimer cette ligne.

  /**
   * Vérifie que h1 et h2 sont égaux, en comparant les fréquences Double
   * avec la précision d. Echoue si ce n'est pas le cas.
   * @param h1 un arbre de Huffman
   * @param h2 un arbre de Huffman
   * @param d un double
   */
  def assertEqualsHuffman(h1: Huffman, h2: Huffman, d: Double): Unit = {
    (h1, h2) match {
      case (Feuille(f1, c1), Feuille(f2, c2)) => {
        assertEquals(f1, f2, d);
        assertEquals(c1, c2)
      }
      case (Noeud(f1, h11, h12), Noeud(f2, h21, h22)) =>
        assertEquals(f1, f2, d);
        assertEqualsHuffman(h11, h21, d);
        assertEqualsHuffman(h12, h22, d)
      case _ => fail("Les deux arbres n'ont pas la même structure")
    }
  }

  /**
   * @param listh1 une liste d'arbres
   * @param listh2 une liste d'arbres
   * @param d la précision de la comparaison des Double
   * @return est-ce que les deux arbres sont égaux
   */
  def assertEqualsHuffmanList (listh1:List[Huffman], listh2:List[Huffman], d:Double)  : Unit = {
    (listh1, listh2) match {
      case (Nil,Nil) => ()
      
      case (h1::tail1, h2::tail2) =>
        assertEqualsHuffman(h1, h2, d)
        assertEqualsHuffmanList(tail1, tail2, d)
      
      case _ => fail("Les deux listes n'ont pas la même longueur.")
    }
  }

  /**
   * Arbre de Huffman utilisé pour réaliser les tests
   */
  val h: Huffman = Noeud(1, Feuille(0.45, 'a'),
    Noeud(0.55, Feuille(0.19, 'r'),
      Noeud(0.36, Noeud(0.18, Feuille(0.9, 'c'), Feuille(0.9, 'd')),
        Feuille(0.18, 'b'))))
  
  /**
   * Test de decodeSymbolv0
   */
  @Test
  def testDecodeSymbolv0 () {
    
    assertEquals(None, Decodage.decodeSymbolv0(h, Nil))
    assertEquals(None, Decodage.decodeSymbolv0(h, One::Zero::One::Nil))
    assertEquals(Some('r'), Decodage.decodeSymbolv0(h, One::Zero::Nil))
    assertEquals(Some('c'), Decodage.decodeSymbolv0(h, One::One::Zero::Zero::Nil))
    
  }
  
  /**
   * Test de decodeSymbol
   */
  @Test
  def testDecodeSymbol () {
    
    assertEquals((None, Nil), Decodage.decodeSymbol(h, Nil))
    assertEquals((Some('r'), One::Nil), Decodage.decodeSymbol(h, One::Zero::One::Nil))
    assertEquals((Some('r'), Nil), Decodage.decodeSymbol(h, One::Zero::Nil))
    assertEquals((Some('d'), Nil), Decodage.decodeSymbol(h, One::One::Zero::One::Nil))
    
  }
  
  /**
   * Test de decode
   */
  @Test
  def testDecode () {
    
    assertEquals(None, decode(Nil, h))
    assertEquals(Some("abra"), decode(List(Zero, One, One, One, One, Zero, Zero), h))
    
  }
  
  /**
   * Test de premiersBits
   */
  @Test
  def testPremiersBits () {
    
    val ref : List[Bit] = One::Zero::One::Zero::One::Zero::Nil
    assertEquals((Nil, ref), premiersBits(ref, 0))
    assertEquals((One::Nil, Zero::One::Zero::One::Zero::Nil), premiersBits(ref, 1))
    assertEquals((ref, Nil), premiersBits(ref, ref.length))
    
  }
  
  /**
   * Test de lireDescription
   */
  @Test
  def testLireDescription () {
    
    val (arbre, reste) = lireDescription(Utils.stringToListBit("0" + Utils.vers16Bits("a")))
    assertEqualsHuffman(Feuille(0, 'a'), arbre, 0.0001)
    assertEquals(reste, Nil)
    
    val description : String = "10" + Utils.vers16Bits("a") + "10" + Utils.vers16Bits("b") + "0" + Utils.vers16Bits("c")
    val message : List[Bit] = One::Zero::One::Zero::Nil
    val (arbrebis, restebis) = lireDescription(Utils.stringToListBit(description) ++ message)
    assertEqualsHuffman(
        Noeud(0, Feuille(0, 'a'), Noeud(0, Feuille(0, 'b'), Feuille(0, 'c'))),
        arbrebis,
        0.0001
        )
    assertEquals(restebis, message)
    
  }
  
  /**
   * Test de decode (final)
   */
  def testDecodeFinal () {
    
    val texte : String = "1000000000011000011000000000011100101100000000001100011000000000011001000000000000110001001111001100011010111100"
    assertEquals("abracadabra", decode(texte))
    
  }

}






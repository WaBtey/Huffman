package fr.istic.si2.test.huffman

import org.junit.Test
import org.junit.Assert._
import fr.istic.si2.huffman.Encodage._
import fr.istic.si2.huffman.Decodage._
import fr.istic.si2.testerApp._
import fr.istic.si2.huffman._

class TestsEncodage {

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
   * Test de encodeSymbol
   */
  @Test
  def testEncodeSymbol () {
    
    assertEquals(None, Encodage.encodeSymbol('k', h))
    assertEquals(Some(Zero::Nil), Encodage.encodeSymbol('a', h))
    assertEquals(Some(One::One::Zero::Zero::Nil), Encodage.encodeSymbol('c', h))
    assertEquals(Some(One::One::One::Nil), Encodage.encodeSymbol('b', h))
    
  }
  
  /**
   * Test de encodelist
   */
  @Test
  def testEncodelist () {
    
    assertEquals(Nil, Encodage.encodeList(Nil, h))
    assertEquals(Zero::Nil, Encodage.encodeList('a'::Nil, h))
    assertEquals(Zero::One::One::One::One::One::Zero::Zero::Nil, Encodage.encodeList("abc".toList, h))
    assertEquals(Nil, Encodage.encodeList('k'::Nil, h))
    assertEquals(Zero::One::One::Zero::Zero::Nil, Encodage.encodeList("akc".toList, h))
    
  }
  
  /**
   * Test de encode
   */
  @Test
  def testEncode () {
    
    assertEquals(Nil, Encodage.encode("", h))
    assertEquals(Zero::Nil, Encodage.encode("a", h))
    assertEquals(Zero::One::One::One::One::One::Zero::Zero::Nil, Encodage.encode("abc", h))
    assertEquals(Nil, Encodage.encode("k", h))
    assertEquals(Zero::One::One::Zero::Zero::Nil, Encodage.encode("akc", h))
    
  }
  
  /**
   * Test de descriptionHuffman
   */
  @Test
  def testsDescriptionHuffman () {
    
    assertEquals("0" + Utils.vers16Bits("a"), descriptionHuffman(Feuille(1.0, 'a')))
    
    assertEquals(
      "10" + Utils.vers16Bits("a") + "0" + Utils.vers16Bits("b"),
      descriptionHuffman(Noeud(1.0, Feuille(0.5, 'a'), Feuille(0.5, 'b')))
      )
    
   assertEquals(
      "10" + Utils.vers16Bits("a") + "10" + Utils.vers16Bits("b") + "0" + Utils.vers16Bits("c"),
      descriptionHuffman(Noeud(1.0, Feuille(0.5, 'a'), Noeud(0.5, Feuille(0.25, 'b'), Feuille(0.25, 'c'))))
      )
    
  }
  
  /**
   * Test de encode (final)
   */
  @Test
  def testEncodeFinal () {
    
    // val arbre : Huffman = Noeud(1.0, Noeud(0.5, Feuille(0.25, 'b'), Feuille(0.25, 'c')), Feuille(0.5, 'a'))
    val description : String = "110" + Utils.vers16Bits("b") + "0" + Utils.vers16Bits("c")  + "0" + Utils.vers16Bits("a")
    val message : String = "aaabbcac"
    val messageEncode : String = "111000001101"
    
    assertEquals(description + messageEncode, Encodage.encode(message))
    
  }

}

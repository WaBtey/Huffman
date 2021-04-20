package fr.istic.si2.test.huffman

import org.junit.Assert.assertEquals
import org.junit.Assert.fail
import org.junit.Test

import fr.istic.si2.huffman.ConstructionCode
import fr.istic.si2.huffman.Feuille
import fr.istic.si2.huffman.Huffman
import fr.istic.si2.huffman.HuffmanApp0
import fr.istic.si2.huffman.Noeud
import fr.istic.si2.testerApp.AppInit

class TestsCode {

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
   * Test de initHuffman
   */
  @Test
  def testInitHuffman () {
    assertEquals(Nil, ConstructionCode.initHuffman(Nil))
    assertEqualsHuffmanList(List(Feuille(1,'e')), ConstructionCode.initHuffman(('e',1.0)::Nil), 0.0001)
    assertEqualsHuffmanList(
      List(Feuille(0.5,'e'), Feuille(0.5,'g')),
      ConstructionCode.initHuffman(('e',0.5)::('g',0.5)::Nil),
      0.0001
      )
  }
  
  /**
   * Test de partition
   */
  @Test
  def testsPartition () {
    assertEquals((Nil, Nil), ConstructionCode.partition(Nil, Feuille(1, ' ')))
    
    val (a,b) = ConstructionCode.partition(List(Feuille(0.4, ' '), Feuille(0.25,'g'), Feuille(0.35,'e')), Feuille(0.39, '\n'))
    assertEqualsHuffmanList(a, List(Feuille(0.25,'g'), Feuille(0.35,'e')), 0.0001)
    assertEqualsHuffmanList(b, List(Feuille(0.4, ' ')), 0.0001)
  }
  
  /**
   * Test de triSelonFreq
   */
  @Test
  def testTriSelonFreq () {
    assertEqualsHuffmanList(Nil, ConstructionCode.triSelonFreq(Nil), 0.0001)
    assertEqualsHuffmanList(List(Feuille(1,'e')), ConstructionCode.triSelonFreq(List(Feuille(1,'e'))), 0.0001)
    assertEqualsHuffmanList(
      List(Feuille(0.3,'g'), Feuille(0.7,'e')),
      ConstructionCode.triSelonFreq(List(Feuille(0.7,'e'), Feuille(0.3,'g'))),
      0.0001
      )
  }
  
  /**
   * Test de uneFusion
   */
  @Test
  def testsUneFusion () {
    assertEqualsHuffmanList(
      List(Noeud(1, Feuille(0.3,'g'), Feuille(0.7,'e'))),
      ConstructionCode.uneFusion(List(Feuille(0.3,'g'), Feuille(0.7,'e'))),
      0.0001
      )
    
    assertEqualsHuffmanList(
      List(Noeud(1, Feuille(0.3,'g'), Feuille(0.7,'e')), Feuille(0.0, '\n')),
      ConstructionCode.uneFusion(List(Feuille(0.3,'g'), Feuille(0.7,'e'), Feuille(0.0, '\n'))),
      0.0001
      )
  }
  
  /**
   * Test de fusion
   */
  @Test
  def testsFusion () {
    
    assertEqualsHuffman(Feuille(1,'e'), ConstructionCode.fusion(List(Feuille(1,'e'))), 0.0001)
    
    assertEqualsHuffman(
      Noeud(1, Feuille(0.3,'g'), Feuille(0.7,'e')),
      ConstructionCode.fusion(List(Feuille(0.3,'g'), Feuille(0.7,'e'))),
      0.0001
      )
    
    assertEqualsHuffman(
      Noeud(1, Noeud(0.3, Feuille(0.1,'g'), Feuille(0.2,'e')), Feuille(0.7, ' ')),
      ConstructionCode.fusion(List(Feuille(0.1,'g'), Feuille(0.2,'e'), Feuille(0.7, ' '))),
      0.0001
      )
    
      assertEqualsHuffman(
      Noeud(1, Feuille(0.4, ' '), Noeud(0.6, Feuille(0.25,'g'), Feuille(0.35,'e'))),
      ConstructionCode.fusion(List(Feuille(0.4, ' '), Feuille(0.25,'g'), Feuille(0.35,'e'))),
      0.0001
      )
      
  }
  
  /**
   * Test de codeHuffman
   */
  @Test
  def testsCodeHuffman () {
    
    assertEqualsHuffman(
      Noeud(1, Feuille(0.4, ' '), Noeud(0.6, Feuille(0.25,'g'), Feuille(0.35,'e'))),
      ConstructionCode.codeHuffman(List((' ', 0.4), ('g', 0.25), ('e', 0.35))),
      0.0001
      )
      
  }
  
  /**
   * Test de compteEtSupprime
   */
  @Test
  def testsCompteEtSupprime () {
    assertEquals((0, Nil), ConstructionCode.compteEtSupprime('e', Nil))
    assertEquals((2, Nil), ConstructionCode.compteEtSupprime('a', 'a'::'a'::Nil))
    assertEquals((3, "art ".toList), ConstructionCode.compteEtSupprime('e', "aerte e".toList))
  }
  
  /**
   * Test de frequencesChars
   */
  @Test
  def testsFrequencesChars () {
    assertEquals(Nil, ConstructionCode.frequencesChars(Nil, 0))
    assertEquals(('a',1.0)::Nil, ConstructionCode.frequencesChars('a'::'a'::Nil, 2))
    assertEquals(List(('a',1/7.0), ('e', 3/7.0), ('r', 1/7.0), ('t', 1/7.0), (' ', 1/7.0)), ConstructionCode.frequencesChars("aerte e".toList, 7))
  }
  
  /**
   * Test analyseFrequences
   */
  @Test
  def testsAnalyseFrequences () {
    assertEquals(Nil, ConstructionCode.analyseFrequences(""))
    assertEquals(('a',1)::Nil, ConstructionCode.analyseFrequences("aaaaaaaaaaaaaaaa"))
    assertEquals(List(('a',0.5), (' ', 0.25), ('e', 0.25)), ConstructionCode.analyseFrequences("a ae"))
  }
  

}

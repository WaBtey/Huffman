package fr.istic.si2.huffman

import Encodage._
import Decodage._
import ConstructionCode._
import Utils._

/**
 * Application principale V1 : arbre de code fixé
 */
object HuffmanApp1 extends App {

  /**
   * Arbre de code utilisé par l'application principale
   */
  val h: Huffman = Noeud(1, Feuille(0.45, 'a'),
    Noeud(0.55, Feuille(0.19, 'r'),
      Noeud(0.36, Noeud(0.18, Feuille(0.9, 'c'), Feuille(0.9, 'd')),
        Feuille(0.18, 'b'))))
        
//  val h: Huffman =
//    Noeud(1.00, Noeud(0.43, Feuille(0.21, 'b'), 
//    Noeud(0.22, Feuille(0.09, 'e'), Noeud(0.13, Feuille(0.06, 'g'),
//    Feuille(0.07, 'f')))), Noeud(0.57, Feuille(0.25, 'a'),
//    Noeud(0.32, Feuille(0.14, 'd'),
//    Feuille(0.18, 'c'))))

  
  /**
   * @return le test des fonctions encode et decode  
   */
  def testUnString(): Unit = {
    println("Chaîne à encoder ? (char disponible : b,e,g,f,a,d,c)")
    val user: String = scala.io.StdIn.readLine()
    println("Chaîne encodée standard: ")
    val sStandard: String = vers16Bits(user)
    val nStandard: Int = sStandard.length
      println("     " + sStandard)
      println("     taille (nb de Bits): " + nStandard)
    println("Chaîne encodée Huffman: ")
    val eH: List[Bit] = encode(user, h)
    val sH: String = listBitToString(eH)
      println("     " + sH)
    println("Chaîne décodée Huffman: ")
    val odH: Option[String] = decode(eH, h)
    print("     ")
    odH.foreach(println)
  }
  
  testUnString()
  println("Again ? [Y/N]")
  val user: Char = scala.io.StdIn.readChar()
  user match{
    case 'Y' | 'y' => testUnString()
    case _ => println("Au revoir")
  }
        


}
  val user: Char = scala.io.StdIn.readChar()
  user match{
    case 'Y' | 'y' => testUnString()
    case _ => println("Au revoir")
  }
 
}

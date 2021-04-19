package fr.istic.si2.huffman

import Encodage._
import Decodage._
import Utils._

/**
 * Application principale V3 : avec transmission du code
 */
object HuffmanApp3 extends App {

  val encMss: String = encode("merci monsieur")
  println(encMss)
  val decMss: String = decode(encMss)
  println(decMss)
  println()
  println("-----------------------")
  println()
  
  /**
   * @return le test des fonctions encode et decode  
   */
  def testAppv3(): Unit = {
    println("Chaîne à encoder ?")
    val user: String = scala.io.StdIn.readLine()
    println("Chaîne encodée standard: ")
    val sStandard: String = vers16Bits(user)
    val nStandard: Int = sStandard.length
      println("     " + sStandard)
      println("     taille (nb de Bits): " + nStandard)
    println("Rep Huffman + Chaîne encodée Huffman: ")
    val eM: String = encode(user)
      println("     " + eM)
    println("Chaîne décodée Huffman: ")
    val dM: String = decode(eM)
    println("     " + dM)

  }
  
  testAppv3()
  println("Again ? [Y/N]")
  val user: Char = scala.io.StdIn.readChar()
  user match{
    case 'Y' | 'y' => testAppv3()
    case _ => println("Au revoir")
  }
  
  //print(toChar("0000000001101001"))

}
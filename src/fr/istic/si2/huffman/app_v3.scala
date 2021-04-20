package fr.istic.si2.huffman

import Encodage._
import Decodage._
import Utils._

/**
 * Application principale V3 : avec transmission du code
 */
object HuffmanApp3 extends App {

  
  // V3 avec une String
  /**
   * @return le test des fonctions encode et decode AppV3
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
              println()
  }
  
  // V3 avec lecture de .txt

  val nomFichier: String = scala.io.StdIn.readLine("Entrer l'adresse du fichier : ")

  print("Lecture du fichier \"" + nomFichier + "\" ... ")
  val contenu: String = lireFichier(nomFichier)
  println("Fait.")

  val nomDeSortie: String = scala.io.StdIn.readLine("Entrer l'adresse du fichier de sortie : ")
  val choix: String = scala.io.StdIn.readLine("Encoder (e) ou décoder (d) ? ")

  choix match {
    case "e" =>
      print("Encodage ... ")
      val contenuEnc: String = encode(contenu)
      println("Fait.")
      print("Ecriture du fichier dans \"" + nomDeSortie + "\" ... ")
      ecrireFichier(nomDeSortie, contenuEnc)
      println("Fait.")

    case "d" =>
      print("Decodage ... ")
      val contenuDec: String = decode(contenu)
      println("Fait.")
      print("Ecriture du fichier dans \"" + nomDeSortie + "\" ... ")
      ecrireFichier(nomDeSortie, contenuDec)
      println("Fait.")

    case _ => println("Choix invalide.")
  }

}
  
  //print(toChar("0000000001101001"))

}

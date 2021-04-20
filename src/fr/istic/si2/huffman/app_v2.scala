package fr.istic.si2.huffman

import Encodage._
import Decodage._
import ConstructionCode._
import Utils._

/**
 * Application principale V2 : avec construction du code
 */
object HuffmanApp2 extends App {

  /**
   * Une liste de couples caractère / fréquence d'apparition
   * à utiliser par l'application principale.
   */
  val lfreqs: List[(Char, Double)] = List(
    ('a', 0.45),
    ('b', 0.18),
    ('c', 0.09),
    ('d', 0.09),
    ('r', 0.19))
    
//  val lfreqs: List[(Char, Double)] =
//    ('a', 0.45)::('r', 0.19)::('c', 0.09)::('d', 0.09)::('b', 0.18)::Nil

  print("Entrer l'adresse du fichier : ")
  val nomFichier: String = scala.io.StdIn.readLine()
  val contenu: String = lireFichier(nomFichier)
  
  print("Construction de l'arbre de Huffman ... ")
  val huffman: Huffman = codeHuffman(lfreqs)
  // val huffman : Huffman = codeHuffman(analyseFrequences(contenu))
  println("Fait.")
  
  print("Encodage ... ")
  val encodage: String = listBitToString(encode(contenu, huffman))
  println("Fait.")
  
  val nomDeSortie: String = nomFichier + ".encode.txt"
  print("Ecriture du fichier dans \"" + nomDeSortie + "\" ... ")
  ecrireFichier(nomDeSortie, encodage)
  println("Fait.")

}

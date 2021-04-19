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
  val lfreqs: List[(Char, Double)] =
    ('a', 0.45)::('r', 0.19)::('c', 0.09)::('d', 0.09)::('b', 0.18)::Nil
    
  println(triSelonFreq(initHuffman(lfreqs)))
  

  println("---------------")
  
  // TODO V2 - A vous de programmer l'application principale
  val bBit: String = vers16Bits("b")
  print(bBit + " :  ")
  println(toChar(bBit))
  println(toChar("0000000001100010"))
}
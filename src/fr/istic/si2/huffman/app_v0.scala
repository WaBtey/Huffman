package fr.istic.si2.huffman

import Encodage._
import Decodage._
import Utils._

/**
 * Application principale V0 : arbre de code fixé, encodage/décodage de caractères
 */
object HuffmanApp0 extends App {

  /**
   * Arbre de code utilisé par l'application principale
   */
  val h: Huffman =
    Noeud(1.00, Feuille(0.45, 'a'), Noeud(0.55, Feuille(0.19, 'r'),
    Noeud(0.36, Noeud(0.18, Feuille(0.09, 'c'), Feuille(0.09, 'd')),
    Feuille(0.18, 'b'))))

  println(listBitToString(Zero::One::Zero::One::Nil))
  println(encodeSymbol('r', h))
  println(encodeSymbol('p', h))
  println(decodeSymbol(h, Zero::One::Zero::Nil)) //decodeSymbol test
  println(decodeSymbol(h, One::One::Zero::Zero::Nil))
  println("EncodeList invalide: ")
  println(encodeList('a'::'b'::'r'::'o'::'c'::Nil ,h))
  println()
  println("---------------------")
  println()
  
  /**
   * @param c un caractère
   * test les fonctions avec un caractère
   */
  def testUnChar (c : Char) : Unit = {
    print(c + " --> ")
    encodeSymbol(c, h) match {
      case None => print("ERREUR")
      case Some(lb) => print(lb + " --> ")
                       print(listBitToString(lb) + " --> ")
                       decodeSymbolv0(h, lb) match {
                         case None => println("ERREUR")
                         case Some(ch) => println(ch)
                       }
    }
  }
  val a1 =0;
  for( a1 <- 97 to 122) {
    val cA: Char = a1.toChar
    if (encodeSymbol(cA, h) != None) {
      testUnChar(cA)
    }
  }
  println()
  println("---------------------")
  println()
  println("a way to appv1:")
  println()  
  
// Pour ajuster les espaces selon la plus grande *list*
//  for( a <- 97 to 122) {
//    val cA: Char = a.toChar
//    val list: List[Bit] = encode(cA.toString, h)
//    val list_str = list.mkString("List(", ", ", ")")
//    var listLength: Int = 0           //On a beosin d'une variable mutable ici, ou d'un substitue
//    if (list_str.length>listLength) 
//      listLength = list_str.length
//      
//  }
  
  val a =0;
   for( a <- 97 to 122) {
     val cA: Char = a.toChar
     val list: List[Bit] = encode(cA.toString, h)
     if (!list.isEmpty) {
       val list_str = list.mkString("List(", ", ", ")")
       print(cA + " ")
       print(f"$list_str%-30s" + f"${listBitToString(list)}%-10s" )
       val s: Option[Char] = decodeSymbol(h, list)._1
       s.foreach(println)
     }
     
   }

   //Test pour alignement en colonne
//val list = List(1, 2, 3)
//val list2 = List("qsdqls", "qsdhqs", "qsjhdqsjhd")
//val list_str = list.mkString("List(", ", ", ")")
//val list2_str = list2.mkString("List(", ", ", ")")
//
//
//for (x <- list) {
//  print(f"$x%-20d")
//}
//
//print("\n")
//
//for (x <- list2) {
//  print(f"$x%-20s")
//}


  
}






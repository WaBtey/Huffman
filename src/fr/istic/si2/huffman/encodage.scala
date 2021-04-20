package fr.istic.si2.huffman

import Utils._
import ConstructionCode._

object Encodage {

  /**
   * @param c un caractère
   * @param h un arbre de Huffman
   * @return l'encodage de c, selon h (si c est bien présent dans h)
   */
  def encodeSymbol(c: Char, h: Huffman): Option[List[Bit]] = {
    h match {
      case Feuille(_ , char)  =>
        if (c==char) Some(Nil)
        else None
      case Noeud(_, a0, a1)   =>
        encodeSymbol(c, a0) match {
          case Some(list)     => Some(Zero::list)
          case None           => 
            encodeSymbol(c, a1) match {
              case Some(list) => Some(One::list)
              case None       => None
            }
        }
    }
  }
  
  /**
   * @param l une liste de caractères
   * @param h un arbre de Huffman
   * @return la séquence de bits correspondants à
   *         l'encodage selon h des éléments de l, s'il a réussi.
   *         Les caractères pour lesquels l'encodage est impossible sont oubliés
   */
  def encodeList(l: List[Char], h: Huffman): List[Bit] = {
    l match {
      case Nil            => Nil
      case head::tail     =>   //encodeSymbol(head, h)::encodeList(tail, h)
        encodeSymbol(head, h) match {
          case None       => encodeList(tail, h)
                        //=> Nil si char impossible la List[Bit] se stoppe au dernier char possible encodé 
          case Some(list) => list ++ encodeList(tail, h)
        } 
    }
  }

  /**
   * @param s une chaîne de caractères
   * @param h un arbre de Huffman
   * @return l'encodage de s, selon h, en une liste de bits.
   *         (concaténation de l'encodage de chaque caractère de s selon h)
   */
  def encode(s: String, h: Huffman): List[Bit] = encodeList(s.toList, h)

  /**
   * @param h un arbre de Huffman
   * @return une chaîne de 0 et 1 uniquement représentant l'arbre h (voir partie 1.3 de l'énoncé)
   *         Les caractères encodables avec h sont représentés dans leur encodage binaire 16 bits.
   */
  def descriptionHuffman(h: Huffman): String = {
    h match {
      case Feuille(_, c)  => "0" + vers16Bits(c.toString)
      case Noeud(_, g, d) => "1" + descriptionHuffman(g) + descriptionHuffman(d)
    }
  }

  /**
   * @param message une chaîne de caractères
   * @return la chaîne de 0 et 1, contenant:
   *         - la représentation de l'arbre de huffman construit à partir de message
   *         - puis l'encodage de message en utilisant l'arbre construit à partir de message
   */
  def encode(message: String): String = {
    val h: Huffman = codeHuffman(analyseFrequences(message))
    descriptionHuffman(h) + listBitToString(encode(message, h))
  }

}



  /**
   * @param s une chaîne de caractères
   * @param h un arbre de Huffman
   * @return l'encodage de s, selon h, en une liste de bits.
   *         (concaténation de l'encodage de chaque caractère de s selon h)
   */
  def encode(s: String, h: Huffman): List[Bit] = {
    val l: List[Char] = s.toList
    encodeList(l, h)
  }

  /**
   * @param h un arbre de Huffman
   * @return une chaîne de 0 et 1 uniquement représentant l'arbre h (voir partie 1.3 de l'énoncé)
   *         Les caractères encodables avec h sont représentés dans leur encodage binaire 16 bits.
   */
  def descriptionHuffman(h: Huffman): String = {
    h match {
      case Feuille(f, c) => "0" + vers16Bits(c.toString)
      case Noeud(f, h0, h1) =>
        "1" + descriptionHuffman(h0) + descriptionHuffman(h1) 
    }
  }

  /**
   * @param message une chaîne de caractères
   * @return la chaîne de 0 et 1, contenant:
   *         - la représentation de l'arbre de huffman construit à partir de message
   *         - puis l'encodage de message en utilisant l'arbre construit à partir de message
   */
  def encode(message: String): String = {
    val h: Huffman = codeHuffman(analyseFrequences(message))
    descriptionHuffman(h) + listBitToString(encode(message, h))
  }
}

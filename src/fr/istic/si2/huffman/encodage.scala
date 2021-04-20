package fr.istic.si2.huffman

import scala.io.Source
import java.io.{ File, PrintWriter }

object Utils {

  /**
   * @param l une liste de bits
   * @return la chaîne de 0 et 1 où chaque bit de l est représenté par 0 ou 1, dans l'ordre
   */
  def listBitToString(l: List[Bit]): String = {
    l match {
      case Nil          => ""
      case One :: tail  => "1" + listBitToString(tail)
      case Zero :: tail => "0" + listBitToString(tail)
    }
  }

  /**
   * @param s une chaîne de caractères
   * @return la chaîne de 0 et 1 représentant chaque caractère
   *         de s par son encodage sur 16 bits
   */
  def vers16Bits(s: String): String = {
    s.toList.map(c => String.format("%16s", c.toBinaryString).replace(' ', '0')).foldLeft("")((acc, e) => acc + e)
  }

  /**
   * Lit le contenu d'un fichier sur disque.
   *
   * @param nom le nom d'un fichier
   * @return la chaîne contenue dans le fichier nommé nom
   *
   * @note Le nom de fichier peut être indiqué de manière
   *       relative à la racine du projet courant.
   */
  def lireFichier(nom: String): String = {
    val bufferedSource = Source.fromFile(nom)
    val contenu = bufferedSource.getLines.mkString(sys.props("line.separator"))
    bufferedSource.close()
    contenu
  }

  /**
   * Ecrit une chaîne de caractères dans un fichier.
   * Le fichier est écrasé s'il était déjà existant.
   *
   * @param nom le nom du fichier dans lequel on écrit
   * @param contenu la chaîne de caractères à écrire
   *
   * @note Le nom de fichier peut être indiqué de manière
   *       relative à la racine du projet courant.
   */
  def ecrireFichier(nom: String, contenu: String): Unit = {
    val writer = new PrintWriter(new File(nom))
    writer.write(contenu)
    writer.close()
  }

  /**
   * @param s une chaîne de 0 et 1, encodage binaire 16 bits d'un caractère
   * @return le caractère correspondant à s
   */
  def toChar(s: String): Char = {
    Integer.parseInt(s, 2).toChar
  }

  /**
   * @param c un caractère 0 ou 1
   * @return le bit correspondant à c
   */
  def charToBit(c: Char): Bit = {
    c match {
      case '0' => Zero
      case '1' => One
      case _   => sys.error("Unknown bit character: " + c)
    }
  }

  /**
   * @param s une chaîne de 0 et 1 uniquement
   * @return la liste de bits correspondant à s
   */
  def stringToListBit(s: String): List[Bit] = {
    s.toList.map(charToBit)
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

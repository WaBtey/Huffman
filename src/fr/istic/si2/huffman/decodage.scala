package fr.istic.si2.huffman

import Utils._

object Decodage {

  /**
   * @param h un arbre de Huffman
   * @param l une liste de bits
   * @return caractère correspondant au décodage de l selon h
   *          si l est un chemin valide de h
   */
  def decodeSymbolv0(h: Huffman, l: List[Bit]): Option[Char] = {
    (h, l) match {
      case (Feuille(_, c), Nil) => Some(c)
      case (Noeud(a, b, c), Zero::tail)  => decodeSymbolv0(b, tail)
      case (Noeud(a, b, c), One::tail) => decodeSymbolv0(c, tail)
      case (_, _) => None
    }
  }

  /**
   * @param h un arbre de Huffman
   * @param l une liste de bits
   * @return un tuple de taille 2
   *         - première composante : caractère correspondant au décodage selon h d'un préfixe de l
   *         - deuxième composante : la liste des bits restant à décoder
   */
  def decodeSymbol(h: Huffman, l: List[Bit]): (Option[Char], List[Bit]) = {
    (h, l) match {
      case (Noeud(a, b, c), Zero::tail)  => decodeSymbol(b, tail)
      case (Noeud(a, b, c), One::tail) => decodeSymbol(c, tail)
      case (Feuille(_, c), Nil) => (Some(c), Nil)
      case (Feuille(_, c), listF) => (Some(c), listF)
      case (_, _) => (None, l) //param: (Noeud, Nil)
    }
  }

  /**
   * @param l une liste de bits
   * @param h un arbre de Huffman
   * @return la chaîne correspondant au décodage de l, selon h, si elle existe
   */
  def decode(l: List[Bit], h: Huffman): Option[String] = {
    decodeSymbol(h, l) match {
      case (Some(c), Nil) => Some(c.toString)
      case (Some(c), list) => 
        decode(list, h) match {
          case Some(s) => Some(c + s) //longue liste d'attente en mémoire
          case None => None
        }
      case (None, _) => None
    }
  }

  /**
   * @param l une liste de bits décrivant, au moins, la représentation binaire d'un arbre de Huffman
   * @return un tuple de taille 2 comprenant :
   *         - l'arbre de code de Huffman reconstruit à partir du début de l
   *         - le reste de la liste l, après la représentation de l'arbre de Huffman
   */
  def lireDescription(l: List[Bit]): (Huffman, List[Bit]) = {
    l match {
      case Nil => sys.error("Nil list")
      case head::tail =>
        head match {
          case Zero => 
            tail match {
              case a::b::c::d::e::f::g::h::i::j::k::l::m::n::o::p::tail2 => //not the efficient way i guess
                (Feuille(1,
                 toChar(listBitToString(a::b::c::d::e::f::g::h::i::j::k::l::m::n::o::p::Nil))), tail2)
              case Nil => sys.error("Nil list")              
              case _ => sys.error("not enough info")
            }
          case One => 
            val laSuite: (Huffman, List[Bit]) = lireDescription(tail)
            val rightSidelistF: (Huffman, List[Bit]) = lireDescription(laSuite._2)
            (Noeud(1, laSuite._1, rightSidelistF._1), rightSidelistF._2)
        }
    }

  }

  /**
   * @param messageEnc une chaîne de 0 et 1 uniquement, contenant la représentation
   *                   d'un arbre de Huffman, puis un message encodé
   * @return le message décodé contenu dans messageEnc, en utilisant le code de huffman
   *         représenté en début de messageEnc
   */
  def decode(messageEnc: String): String = {
    val listDuMess: List[Bit] = stringToListBit(messageEnc)
    //println(listDuMess)
    val jsp: (Huffman, List[Bit]) = lireDescription(listDuMess)
    decode(jsp._2, jsp._1) match {
      case Some(m) => m
      case None => sys.error("Décodage Impossible")
    }
  }

}
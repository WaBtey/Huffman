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
      case (Feuille(_, c), Nil)                => Some(c)
      case (Noeud(_, zero, one), Zero :: tail) => decodeSymbolv0(zero, tail)
      case (Noeud(_, zero, one), One :: tail)  => decodeSymbolv0(one, tail)
      case _                                   => None
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
      case (Feuille(_, c), Nil)                => (Some(c), Nil)
      case (Feuille(_, c), tail)               => (Some(c), tail)
      case (Noeud(_, zero, one), Zero :: tail) => decodeSymbol(zero, tail)
      case (Noeud(_, zero, one), One :: tail)  => decodeSymbol(one, tail)
      case _                                   => (None, l)              //param: (Noeud, Nil)
    }
  }

  /**
   * @param l une liste de bits
   * @param h un arbre de Huffman
   * @return la chaîne correspondant au décodage de l, selon h, si elle existe
   * @note si une suite de bits ne correspond à aucun caractère, la fonction retourne None
   */
  def decode(l: List[Bit], h: Huffman): Option[String] = {
    decodeSymbol(h, l) match {
      case (None, _)       => None
      case (Some(c), Nil)  => Some(c.toString)
      case (Some(c), tail) =>
        decode(tail, h) match {
          case None    => None
          case Some(s) => Some(c + s)  //longue liste d'attente en mémoire
        }
    }
  }

  /**
   * @param l une liste de Bits d'au moins n bits
   * @param n un entier positif ou nul
   * @return les n premiers bits et le reste de la liste
   */
  def premiersBits(l: List[Bit], n: Int): (List[Bit], List[Bit]) = {
    (n, l) match {
      case (0, _) => (Nil, l)

      case (_, head :: tail) =>
        val (premiers, reste) = premiersBits(tail, n - 1)
        (head :: premiers, reste)

      case _ => scala.sys.error("l est vide mais n > 0")
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
      case Zero :: tail =>
        val (chr, reste) = premiersBits(tail, 16)
        (Feuille(0, toChar(listBitToString(chr))), reste)

      case One :: tail =>
        val (huffd, reste) = lireDescription(tail)
        val (huffg, reminder) = lireDescription(reste)
        (Noeud(0, huffd, huffg), reminder)

      case _ => scala.sys.error("La longueur de l ne doit pas être vide.")
    }
  }
  
//  /**
//   * @param l une liste de bits décrivant, au moins, la représentation binaire d'un arbre de Huffman
//   * @return un tuple de taille 2 comprenant :
//   *         - l'arbre de code de Huffman reconstruit à partir du début de l
//   *         - le reste de la liste l, après la représentation de l'arbre de Huffman
//   */
//  def lireDescription(l: List[Bit]): (Huffman, List[Bit]) = {
//    l match {
//      case Nil => sys.error("Nil list")
//      case head::tail =>
//        head match {
//          case Zero => 
//            tail match {
//              case a::b::c::d::e::f::g::h::i::j::k::l::m::n::o::p::tail2 => //not the efficient way i guess
//                (Feuille(1,
//                 toChar(listBitToString(a::b::c::d::e::f::g::h::i::j::k::l::m::n::o::p::Nil))), tail2)
//              case Nil => sys.error("Nil list")              
//              case _ => sys.error("not enough info")
//            }
//          case One => 
//            val laSuite: (Huffman, List[Bit]) = lireDescription(tail)
//            val rightSidelistF: (Huffman, List[Bit]) = lireDescription(laSuite._2)
//            (Noeud(1, laSuite._1, rightSidelistF._1), rightSidelistF._2)
//        }
//    }
//
//  }

  /**
   * @param messageEnc une chaîne de 0 et 1 uniquement, contenant la représentation
   *                   d'un arbre de Huffman, puis un message encodé
   * @return le message décodé contenu dans messageEnc, en utilisant le code de huffman
   *         représenté en début de messageEnc
   */
  def decode(messageEnc: String): String = {
    val (huffman, message) = lireDescription(stringToListBit(messageEnc))
    decode(message, huffman) match {
      case Some(sd) => sd
      case None      => sys.error("Impossible de décoder le message.")
    }
  }

}






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

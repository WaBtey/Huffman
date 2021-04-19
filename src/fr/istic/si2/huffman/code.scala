package fr.istic.si2.huffman

object ConstructionCode {

  /**
   * @param l une liste de couples caractère/fréquence
   * @return la liste des arbres de Huffman réduits à des feuilles,
   *         un pour chaque élément de l
   */
  def initHuffman(l: List[(Char, Double)]): List[Huffman] = {
    l match {
      case Nil            => Nil
      case (c, f) :: tail => Feuille(f, c) :: initHuffman(tail)
    }
  }

  /**
   * @param l une liste d'entiers
   * @param pivot un entier
   * @return un couple de deux listes d'entiers (l1,l2) telle que
   *         chaque élément de l se retrouve soit dans l1, soit dans l2.
   *         La taille de l est égale à l1.length + l2.length.
   *         Chaque élément de l1 est strictement inférieur à a.
   *         Chaque élément de l2 est supérieur ou égal à a.
   */
  def partition(l: List[Huffman], pivot: Huffman): (List[Huffman], List[Huffman]) = {
    l match {
      case Nil => (Nil, Nil)
      case Feuille(freq, c) :: tail => (partition(tail, pivot), pivot) match {
        case ((l1, l2), Feuille(f, _)) =>
          if (freq < f) (Feuille(freq, c) :: l1, l2)
          else (l1, Feuille(freq, c) :: l2)

        case ((l1, l2), Noeud(f, _, _)) =>
          if (freq < f) (Feuille(freq, c) :: l1, l2)
          else (l1, Feuille(freq, c) :: l2)
      }
      case Noeud(freq, zero, one) :: tail => (partition(tail, pivot), pivot) match {
        case ((l1, l2), Feuille(f, _)) =>
          if (freq < f) (Noeud(freq, zero, one) :: l1, l2)
          else (l1, Noeud(freq, zero, one) :: l2)

        case ((l1, l2), Noeud(f, _, _)) =>
          if (freq < f) (Noeud(freq, zero, one) :: l1, l2)
          else (l1, Noeud(freq, zero, one) :: l2)
      }
    }
  }

  /**
   * @param l une liste d'arbres de Huffman
   * @return la liste des éléments de l, classée par ordre croissant des fréquences aux racines
   */
  def triSelonFreq(l: List[Huffman]): List[Huffman] = {
    l match {
      case Nil => Nil
      case head :: tail => partition(tail, head) match {
        case (l1, l2) => triSelonFreq(l1) ++ (head :: triSelonFreq(l2))
      }
    }
  }

  /**
   * @param l une liste d'arbres de Huffman, de longueur au moins 2
   * @return la liste obtenue après avoir fusionné les 2 arbres de l de fréquences minimales
   */
  def uneFusion(l: List[Huffman]): List[Huffman] = {
    l match {
      case Feuille(f1, c1) :: Feuille(f2, c2) :: tail => Noeud(f1 + f2, Feuille(f1, c1), Feuille(f2, c2)) :: tail
      case Noeud(f1, g1, d1) :: Feuille(f2, c2) :: tail => Noeud(f1 + f2, Noeud(f1, g1, d1), Feuille(f2, c2)) :: tail
      case Feuille(f1, c1) :: Noeud(f2, g2, d2) :: tail => Noeud(f1 + f2, Feuille(f1, c1), Noeud(f2, g2, d2)) :: tail
      case Noeud(f1, g1, d1) :: Noeud(f2, g2, d2) :: tail => Noeud(f1 + f2, Noeud(f1, g1, d1), Noeud(f2, g2, d2)) :: tail
      case _ => sys.error("l est de longueur inférieure à 2")
    }
  }

  /**
   * @param l une liste NON VIDE d'arbres de Huffman.
   * @return l'arbre de Huffman obtenu en fusionnant successivement,
   *         et 2 par 2, les arbres de l de fréquences minimales
   */
  def fusion(l: List[Huffman]): Huffman = {
    l match {
      case huffman :: Nil => huffman
      case _              => fusion(uneFusion(triSelonFreq(l)))
    }
  }

  /**
   * @param freqs une liste de couples caractère/fréquence
   * @return l'arbre de code de Huffman correspondant à freqs
   */
  def codeHuffman(freqs: List[(Char, Double)]): Huffman = {
    fusion(initHuffman(freqs))
  }

  /**
   * @param c le char à compter et à supprimer
   * @param l la liste de chars dans laquelle on doit compter
   * @return le nombre de c dans l et le reste de la liste sans c
   */
  def compteEtSupprime(c: Char, l: List[Char]): (Int, List[Char]) = {
    l match {
      case Nil => (0, Nil)
      case head :: tail => compteEtSupprime(c, tail) match {
        case (count, reste) =>
          if (head == c)
            (count + 1, reste)
          else
            (count, head :: reste)
      }
    }
  }

  /**
   * @param l une liste de chars
   * @return le nombre d'occurences pour chaque chars
   */
  def compteTousChars(l: List[Char]): List[(Char, Int)] = {
    l match {
      case Nil => Nil
      case head :: tail =>
        compteEtSupprime(head, l) match {
          case (count, reste) => (head, count) :: compteTousChars(reste)
        }
    }
  }

  /**
   * @param s une chaîne de caractères
   * @return la liste des couples (caractère, fréquence d'apparition),
   *         calculée à partir de s. Chaque élément couple (c, f) est tel que
   *         c est un caractère apparaissant dans s, et f est sa fréquence
   *         d'apparition dans s.
   */
  // V2 - On rappelle que maChaine.toList convertit la chaîne maChaine en la liste de ses caractères.
  def analyseFrequences(s: String): List[(Char, Double)] = {
    val total: Double = s.length.toDouble ;
    val occ: List[(Char, Int)] = compteTousChars(s.toList)
    occ.map({ case (chr,n) => (chr, n.toDouble / total.toDouble) })
  }

}





package recursivity

import scala.annotation.tailrec

object Parenthesis {

  def balance(chars: List[Char]): Boolean =
    {

      def checkBalance(compteur: Int, chaine: List[Char]): Boolean = {
        if (compteur < 0) 
        {
          false
        }
        else if (chaine.isEmpty) 
        {
          if(compteur==0)
          {
            true
          }
          else
          {
            false
          } 
        } 
        else 
        {
          /*
          if (chaine.head == '(')
            checkBalance(compteur + 1, chaine.tail)
          else if (chaine.head == ')')
            checkBalance(compteur - 1, chaine.tail)
          else
            checkBalance(compteur, chaine.tail)
            */
          //http://worldline.github.io/scala-cheatsheet/#pattern-matching
          chaine.head match {
            case '(' => checkBalance(compteur+1, chaine.tail)
            case ')' => checkBalance(compteur-1, chaine.tail)
            case none => checkBalance(compteur, chaine.tail)
          }
        }
      }

      checkBalance(0, chars)
    }
}
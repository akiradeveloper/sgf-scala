package sgf

import scala.util.parsing.combinator._

class SGF extends RegexParsers { 
  import sgf._

  def concat(xs: List[String]) = xs.fold(""){(acc,e) => acc+e}

  def pAll = pCollection
  def pCollection = pGameTree.+ ^^ { Collection(_) }
  def pGameTree: Parser[GameTree] = "(" ~ pSequence ~ pGameTree.* ~ ")" ^^ { case "(" ~ seq ~ gts ~ ")" => { GameTree(seq, gts) } }
  def pSequence = pNode.+ ^^ { Sequence(_) }
  def pNode = ";" ~> pProperty.* ^^ { Node(_) }
  def pPropIdent = pUcLetter.+ ^^ { case xs => PropIdent(concat(xs)) }
  def pProperty =
    pPropIdent into {
      case pid @ PropIdent(id) => {
        def prop1[T <: CValueType](p: Parser[T]) = "[" ~> p <~ "]" ^^ { case x => Property(pid, List(PropValue(x))) }
        def propList[T <: CValueType](p: Parser[T]) = ("[" ~> p <~ "]").+ ^^ { case xs => Property(pid, xs.map {PropValue(_)}) }
        def propElist[T <: CValueType](p: Parser[T]) = ("[" ~> p <~ "]").* ^^ { case xs => Property(pid, xs.map {PropValue(_)}) }
        def propListOfPoint = propList(pPoint) // TODO compressed
        def propEListOfPoint = propElist(pPoint)
        id match {
          case "FF" => prop1(pNumber)
          case "HA" => prop1(pNumber)
          case "KM" => prop1(pReal)
          case "AB" => propListOfPoint
          case "AE" => propListOfPoint
          case "AW" => propListOfPoint
          case "PL" => prop1(pColor)
          case "BL" => prop1(pReal)
          case "OB" => prop1(pNumber)
          case "OW" => prop1(pNumber)
          case "WL" => prop1(pReal)

          // Markup
          case "AR" => propList(pCompose(pPoint, pPoint))
          case "CR" => propList(pPoint)
          case "VW" => propList(pCompose(pPoint, pSimpleText))
          case "LN" => propList(pCompose(pPoint, pPoint))
          case "NA" => propList(pPoint)
          case "SL" => propList(pPoint)
          case "SQ" => propList(pPoint)
          case "TR" => propList(pPoint)
        }
      }
    }

  def pSimpleText = """\w+""".r ^^ { SimpleText(_) }
  def pNone = "" ^^ { _ => None }
  def pUcLetter = """[A-Z]""".r
  def pDigit = """[0-9]""".r
  def pNumber_ = ("+"|"-"|"") ~ rep1(pDigit) ^^ {
    case sig ~ digits => {
      val a: Int = sig match {
        case "+" => 1
        case "-" => -1
        case "" => 1
      }
      val b: Int = concat(digits).toInt
      a * b
    }
  }
  def pNumber = pNumber_ ^^ { Number(_) }
  def pReal = pNumber_ ~ "." ~ rep1(pDigit) ^^ {
    case int ~ "." ~ decimal => {
      val a: Int = int
      val b: Float = ("0." + concat(decimal)).toFloat
      Real(a + b)
    }
  }
  def pDouble = ("1"|"2") ^^ { case x => Double(x.toInt) }
  def pColor = ("B"|"W") ^^ { case x => Color(x.charAt(0)) }
  def pPoint = repN(2, """([a-z]|[A-Z])""".r) ^^ { case List(a, b) => Point(a.head, b.head) }
  def intsof(p: Point) = {
    val Point(x, y) = p
    (x-'a', y-'a')
  }
  def pCompose[A <: ValueType, B <: ValueType](pa: Parser[A], pb: Parser[B]) = pa ~ ":" ~ pb ^^ { case x ~ ":" ~ y => Compose(x, y) }
}

package sgf {
  case class Collection(a: List[GameTree])
  case class GameTree(a: Sequence, b: List[GameTree])
  case class Sequence(a: List[Node])
  case class Node(a: List[Property])

  case class Property(a: PropIdent, b: List[PropValue])
  case class PropIdent(a: String)
  case class PropValue(a: CValueType)

  trait CValueType
  trait ValueType extends CValueType
  case class Compose(a: ValueType, b: ValueType) extends CValueType

  case object None extends ValueType
  case class Number(a: Int) extends ValueType
  case class Real(a: Float) extends ValueType
  case class Double(a: Int) extends ValueType
  case class Color(a: Char) extends ValueType
  case class SimpleText(a: String) extends ValueType
  case class Text(a: String) extends ValueType
  case class Point(a: Char, b: Char) extends ValueType
  case class Move() extends ValueType
  case class Stone() extends ValueType
}

object SGF extends SGF {
  def main(args: Array[String]) = {
    println(parseAll(pNone, ""))
    println(parseAll(pDouble, "2"))
    println(parseAll(pColor, "W"))
    println(parseAll(pNumber, "-23"))
    println(parseAll(pReal, "3.14"))
    // println(parseAll(pAll, "00"))
    println(parse(pProperty, "AB[ab][cd]"))
    println(parse(pPoint, "ab"))
    println(parse(pPropIdent, "KM[+0.5]"))
    println(parseAll(pProperty, "KM[+0.5]"))
    println(parseAll(pAll, "(;KM[+0.5])"))
  }
}

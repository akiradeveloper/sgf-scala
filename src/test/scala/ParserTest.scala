import sgf._
import org.scalatest._

class PropertyTest extends FunSuite {
  import sgf._

  def t(x: String, xs: CValueType*) = {
    val expected = xs.toList.map(PropValue(_))
    val res = SGF.parseAll(SGF.pProperty, x)
    if (res.successful) {
      val Property(_, actual) = res.get
      assert(actual === expected)
    } else {
      println(res)
      assert(false)
    }
  }
  test("B") {
    t("B[ ac]", Point('a','c'))
  }
  test("VW") { // elist of point
    t("VW[]")
    t("VW[  ]")
    t("VW[ab ][ AB ]", Point('a','b'), Point('A','B'))
    t("VW[ az:CD]", Compose(Point('a','z'), Point('C','D')))
  }
  test("KO") {
    t("KO[     ]")
  }
  test("PL") {
    t("PL[B]", Color('B'))
    t("PL[W]", Color('W'))
  }
  test("FF") {
    t("FF[ +1 ]", Number(1))
    t("FF[ 4]", Number(4))
    t("FF[ -3 ]", Number(-3))
  }
  test("TE") {
    t("TE[  1]", Double(1))
    t("TE[2 ]", Double(2))
  }
  test("V") {
    t("V[3.14]", Real(3.14))
    t("V[+3.14 ]", Real(3.14))
    t("V[ -3.14 ]", Real(-3.14))

    t("V[1]", Real(1))
    t("V[ +1]", Real(1))
    t("V[ -1 ]", Real(-1))
  }
  test("AP") {
    t("AP[Primiview:3.1]", Compose(SimpleText("Primiview"), SimpleText("3.1")))
  }
  test("FG") {
    t("FG[257:Figure 1]", Compose(Number(257), SimpleText("Figure 1")))
  }
}

class ParseFileTest extends FunSuite {
  import sgf._

  import scala.io.Source

  def t(x: String) = {
    test(x + ".sgf") {
      val s = Source.fromURL(getClass.getResource("/" + x + ".sgf")).mkString
      val res = SGF.parseAll(SGF.pAll, s)
      if (res.successful) {
        assert(true)
      } else {
        println(res)
        assert(false)
      }
    }
  }

  t("minimum")
  t("ff4_ex")
  t("print1")
  t("print2")
}

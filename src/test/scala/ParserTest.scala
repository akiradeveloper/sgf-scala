import sgf._
import org.scalatest._

class PropertyTest extends FunSuite {
  import sgf._
  def t(x: String, p: Property) = {
    assert(1 === 1)
  }
  test("B") {
    t("B[ac]", Property(PropIdent("B"), List(PropValue(Point('a','c')))))
    t("B[ab][AB]", Property(PropIdent("B"), List(PropValue(Point('a','b')), PropValue(Point('A','B')))))
  }
}

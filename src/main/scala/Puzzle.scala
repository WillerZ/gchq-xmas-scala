/**
  * Created by philwill on 10/12/2015.
  */
case object ConstraintBuilding {

  implicit class Constraint1(val c1: Int) {
    def ~(c: Int) = new ConstraintX(List(c1, c))
  }

  class ConstraintX(val constraints: List[Int]) {
    def ~(c: Int) = new ConstraintX(constraints :+ c)

    def \\(constraintX: ConstraintX) = new ConstraintsX(List(this, constraintX))
  }

  class ConstraintsX(val constraints: List[ConstraintX]) {
    def \\(constraintX: ConstraintX) = new ConstraintsX(constraints :+ constraintX)
  }

  final val horizontals: ConstraintsX = {
    (7 ~ 3 ~ 1 ~ 1 ~ 7) \\
      (1 ~ 1 ~ 2 ~ 2 ~ 1 ~ 1) \\
      (1 ~ 3 ~ 1 ~ 3 ~ 1 ~ 1 ~ 3 ~ 1) \\
      (1 ~ 3 ~ 1 ~ 1 ~ 6 ~ 1 ~ 3 ~ 1) \\
      (1 ~ 3 ~ 1 ~ 5 ~ 2 ~ 1 ~ 3 ~ 1) \\
      (1 ~ 1 ~ 2 ~ 1 ~ 1) \\
      (7 ~ 1 ~ 1 ~ 1 ~ 1 ~ 1 ~ 7) \\
      (3 ~ 3) \\
      (1 ~ 2 ~ 3 ~ 1 ~ 1 ~ 3 ~ 1 ~ 1 ~ 2) \\
      (1 ~ 1 ~ 3 ~ 2 ~ 1 ~ 1) \\
      (4 ~ 1 ~ 4 ~ 2 ~ 1 ~ 2) \\
      (1 ~ 1 ~ 1 ~ 1 ~ 1 ~ 4 ~ 1 ~ 3) \\
      (2 ~ 1 ~ 1 ~ 1 ~ 2 ~ 5) \\
      (3 ~ 2 ~ 2 ~ 6 ~ 3 ~ 1) \\
      (1 ~ 9 ~ 1 ~ 1 ~ 2 ~ 1) \\
      (2 ~ 1 ~ 2 ~ 2 ~ 3 ~ 1) \\
      (3 ~ 1 ~ 1 ~ 1 ~ 1 ~ 5 ~ 1) \\
      (1 ~ 2 ~ 2 ~ 5) \\
      (7 ~ 1 ~ 2 ~ 1 ~ 1 ~ 1 ~ 3) \\
      (1 ~ 1 ~ 2 ~ 1 ~ 2 ~ 2 ~ 1) \\
      (1 ~ 3 ~ 1 ~ 4 ~ 5 ~ 1) \\
      (1 ~ 3 ~ 1 ~ 3 ~ 10 ~ 2) \\
      (1 ~ 3 ~ 1 ~ 1 ~ 6 ~ 6) \\
      (1 ~ 1 ~ 2 ~ 1 ~ 1 ~ 2) \\
      (7 ~ 2 ~ 1 ~ 2 ~ 5)
  }

  final val verticals: ConstraintsX = {
    (7 ~ 2 ~ 1 ~ 1 ~ 7) \\
      (1 ~ 1 ~ 2 ~ 2 ~ 1 ~ 1) \\
      (1 ~ 3 ~ 1 ~ 3 ~ 1 ~ 3 ~ 1 ~ 3 ~ 1) \\
      (1 ~ 3 ~ 1 ~ 1 ~ 5 ~ 1 ~ 3 ~ 1) \\
      (1 ~ 3 ~ 1 ~ 1 ~ 4 ~ 1 ~ 3 ~ 1) \\
      (1 ~ 1 ~ 1 ~ 2 ~ 1 ~ 1) \\
      (7 ~ 1 ~ 1 ~ 1 ~ 1 ~ 1 ~ 7) \\
      (1 ~ 1 ~ 3) \\
      (2 ~ 1 ~ 2 ~ 1 ~ 8 ~ 2 ~ 1) \\
      (2 ~ 2 ~ 1 ~ 2 ~ 1 ~ 1 ~ 1 ~ 2) \\
      (1 ~ 7 ~ 3 ~ 2 ~ 1) \\
      (1 ~ 2 ~ 3 ~ 1 ~ 1 ~ 1 ~ 1 ~ 1) \\
      (4 ~ 1 ~ 1 ~ 2 ~ 6) \\
      (3 ~ 3 ~ 1 ~ 1 ~ 1 ~ 3 ~ 1) \\
      (1 ~ 2 ~ 5 ~ 2 ~ 2) \\
      (2 ~ 2 ~ 1 ~ 1 ~ 1 ~ 1 ~ 1 ~ 2 ~ 1) \\
      (1 ~ 3 ~ 3 ~ 2 ~ 1 ~ 8 ~ 1) \\
      (6 ~ 2 ~ 1) \\
      (7 ~ 1 ~ 4 ~ 1 ~ 1 ~ 3) \\
      (1 ~ 1 ~ 1 ~ 1 ~ 4) \\
      (1 ~ 3 ~ 1 ~ 3 ~ 7 ~ 1) \\
      (1 ~ 3 ~ 1 ~ 1 ~ 1 ~ 2 ~ 1 ~ 1 ~ 4) \\
      (1 ~ 3 ~ 1 ~ 4 ~ 3 ~ 3) \\
      (1 ~ 1 ~ 2 ~ 2 ~ 2 ~ 6 ~ 1) \\
      (7 ~ 1 ~ 3 ~ 2 ~ 1 ~ 1)
  }
}

case object PuzzleBuilder {

  import ConstraintBuilding.{ConstraintX, ConstraintsX}

  sealed trait PuzzleElement {
    def |(x: PuzzleElement) = new Row2(this, x)
  }

  class Row2(e1: PuzzleElement, e2: PuzzleElement) {
    def |(e: PuzzleElement) = new Row3(e1, e2, e)
  }

  class Row3(e1: PuzzleElement, e2: PuzzleElement, e3: PuzzleElement) {
    def |(e: PuzzleElement) = new Row4(e1, e2, e3, e)
  }

  class Row4(e1: PuzzleElement, e2: PuzzleElement, e3: PuzzleElement, e4: PuzzleElement) {
    def |(e: PuzzleElement) = new Row5(e1, e2, e3, e4, e)
  }

  class Row5(e1: PuzzleElement, e2: PuzzleElement, e3: PuzzleElement, e4: PuzzleElement, e5: PuzzleElement) {
    def |(e: PuzzleElement) = new Row6(e1, e2, e3, e4, e5, e)
  }

  class Row6(e1: PuzzleElement, e2: PuzzleElement, e3: PuzzleElement, e4: PuzzleElement, e5: PuzzleElement, e6: PuzzleElement) {
    def |(e: PuzzleElement) = new Row7(e1, e2, e3, e4, e5, e6, e)
  }

  class Row7(e1: PuzzleElement, e2: PuzzleElement, e3: PuzzleElement, e4: PuzzleElement, e5: PuzzleElement, e6: PuzzleElement, e7: PuzzleElement) {
    def |(e: PuzzleElement) = new Row8(e1, e2, e3, e4, e5, e6, e7, e)
  }

  class Row8(e1: PuzzleElement, e2: PuzzleElement, e3: PuzzleElement, e4: PuzzleElement, e5: PuzzleElement, e6: PuzzleElement, e7: PuzzleElement, e8: PuzzleElement) {
    def |(e: PuzzleElement) = new Row9(e1, e2, e3, e4, e5, e6, e7, e8, e)
  }

  class Row9(e1: PuzzleElement, e2: PuzzleElement, e3: PuzzleElement, e4: PuzzleElement, e5: PuzzleElement, e6: PuzzleElement, e7: PuzzleElement, e8: PuzzleElement, e9: PuzzleElement) {
    def |(e: PuzzleElement) = new Row10(e1, e2, e3, e4, e5, e6, e7, e8, e9, e)
  }

  class Row10(e1: PuzzleElement, e2: PuzzleElement, e3: PuzzleElement, e4: PuzzleElement, e5: PuzzleElement, e6: PuzzleElement, e7: PuzzleElement, e8: PuzzleElement, e9: PuzzleElement, e10: PuzzleElement) {
    def |(e: PuzzleElement) = new Row11(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e)
  }

  class Row11(e1: PuzzleElement, e2: PuzzleElement, e3: PuzzleElement, e4: PuzzleElement, e5: PuzzleElement, e6: PuzzleElement, e7: PuzzleElement, e8: PuzzleElement, e9: PuzzleElement, e10: PuzzleElement, e11: PuzzleElement) {
    def |(e: PuzzleElement) = new Row12(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e)
  }

  class Row12(e1: PuzzleElement, e2: PuzzleElement, e3: PuzzleElement, e4: PuzzleElement, e5: PuzzleElement, e6: PuzzleElement, e7: PuzzleElement, e8: PuzzleElement, e9: PuzzleElement, e10: PuzzleElement, e11: PuzzleElement, e12: PuzzleElement) {
    def |(e: PuzzleElement) = new Row13(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e)
  }

  class Row13(e1: PuzzleElement, e2: PuzzleElement, e3: PuzzleElement, e4: PuzzleElement, e5: PuzzleElement, e6: PuzzleElement, e7: PuzzleElement, e8: PuzzleElement, e9: PuzzleElement, e10: PuzzleElement, e11: PuzzleElement, e12: PuzzleElement, e13: PuzzleElement) {
    def |(e: PuzzleElement) = new Row14(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e)
  }

  class Row14(e1: PuzzleElement, e2: PuzzleElement, e3: PuzzleElement, e4: PuzzleElement, e5: PuzzleElement, e6: PuzzleElement, e7: PuzzleElement, e8: PuzzleElement, e9: PuzzleElement, e10: PuzzleElement, e11: PuzzleElement, e12: PuzzleElement, e13: PuzzleElement, e14: PuzzleElement) {
    def |(e: PuzzleElement) = new Row15(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e)
  }

  class Row15(e1: PuzzleElement, e2: PuzzleElement, e3: PuzzleElement, e4: PuzzleElement, e5: PuzzleElement, e6: PuzzleElement, e7: PuzzleElement, e8: PuzzleElement, e9: PuzzleElement, e10: PuzzleElement, e11: PuzzleElement, e12: PuzzleElement, e13: PuzzleElement, e14: PuzzleElement, e15: PuzzleElement) {
    def |(e: PuzzleElement) = new Row16(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e)
  }

  class Row16(e1: PuzzleElement, e2: PuzzleElement, e3: PuzzleElement, e4: PuzzleElement, e5: PuzzleElement, e6: PuzzleElement, e7: PuzzleElement, e8: PuzzleElement, e9: PuzzleElement, e10: PuzzleElement, e11: PuzzleElement, e12: PuzzleElement, e13: PuzzleElement, e14: PuzzleElement, e15: PuzzleElement, e16: PuzzleElement) {
    def |(e: PuzzleElement) = new Row17(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e)
  }

  class Row17(e1: PuzzleElement, e2: PuzzleElement, e3: PuzzleElement, e4: PuzzleElement, e5: PuzzleElement, e6: PuzzleElement, e7: PuzzleElement, e8: PuzzleElement, e9: PuzzleElement, e10: PuzzleElement, e11: PuzzleElement, e12: PuzzleElement, e13: PuzzleElement, e14: PuzzleElement, e15: PuzzleElement, e16: PuzzleElement, e17: PuzzleElement) {
    def |(e: PuzzleElement) = new Row18(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e17, e)
  }

  class Row18(e1: PuzzleElement, e2: PuzzleElement, e3: PuzzleElement, e4: PuzzleElement, e5: PuzzleElement, e6: PuzzleElement, e7: PuzzleElement, e8: PuzzleElement, e9: PuzzleElement, e10: PuzzleElement, e11: PuzzleElement, e12: PuzzleElement, e13: PuzzleElement, e14: PuzzleElement, e15: PuzzleElement, e16: PuzzleElement, e17: PuzzleElement, e18: PuzzleElement) {
    def |(e: PuzzleElement) = new Row19(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e17, e18, e)
  }

  class Row19(e1: PuzzleElement, e2: PuzzleElement, e3: PuzzleElement, e4: PuzzleElement, e5: PuzzleElement, e6: PuzzleElement, e7: PuzzleElement, e8: PuzzleElement, e9: PuzzleElement, e10: PuzzleElement, e11: PuzzleElement, e12: PuzzleElement, e13: PuzzleElement, e14: PuzzleElement, e15: PuzzleElement, e16: PuzzleElement, e17: PuzzleElement, e18: PuzzleElement, e19: PuzzleElement) {
    def |(e: PuzzleElement) = new Row20(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e17, e18, e19, e)
  }

  class Row20(e1: PuzzleElement, e2: PuzzleElement, e3: PuzzleElement, e4: PuzzleElement, e5: PuzzleElement, e6: PuzzleElement, e7: PuzzleElement, e8: PuzzleElement, e9: PuzzleElement, e10: PuzzleElement, e11: PuzzleElement, e12: PuzzleElement, e13: PuzzleElement, e14: PuzzleElement, e15: PuzzleElement, e16: PuzzleElement, e17: PuzzleElement, e18: PuzzleElement, e19: PuzzleElement, e20: PuzzleElement) {
    def |(e: PuzzleElement) = new Row21(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e17, e18, e19, e20, e)
  }

  class Row21(e1: PuzzleElement, e2: PuzzleElement, e3: PuzzleElement, e4: PuzzleElement, e5: PuzzleElement, e6: PuzzleElement, e7: PuzzleElement, e8: PuzzleElement, e9: PuzzleElement, e10: PuzzleElement, e11: PuzzleElement, e12: PuzzleElement, e13: PuzzleElement, e14: PuzzleElement, e15: PuzzleElement, e16: PuzzleElement, e17: PuzzleElement, e18: PuzzleElement, e19: PuzzleElement, e20: PuzzleElement, e21: PuzzleElement) {
    def |(e: PuzzleElement) = new Row22(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e17, e18, e19, e20, e21, e)
  }

  class Row22(e1: PuzzleElement, e2: PuzzleElement, e3: PuzzleElement, e4: PuzzleElement, e5: PuzzleElement, e6: PuzzleElement, e7: PuzzleElement, e8: PuzzleElement, e9: PuzzleElement, e10: PuzzleElement, e11: PuzzleElement, e12: PuzzleElement, e13: PuzzleElement, e14: PuzzleElement, e15: PuzzleElement, e16: PuzzleElement, e17: PuzzleElement, e18: PuzzleElement, e19: PuzzleElement, e20: PuzzleElement, e21: PuzzleElement, e22: PuzzleElement) {
    def |(e: PuzzleElement) = new Row23(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e17, e18, e19, e20, e21, e22, e)
  }

  class Row23(e1: PuzzleElement, e2: PuzzleElement, e3: PuzzleElement, e4: PuzzleElement, e5: PuzzleElement, e6: PuzzleElement, e7: PuzzleElement, e8: PuzzleElement, e9: PuzzleElement, e10: PuzzleElement, e11: PuzzleElement, e12: PuzzleElement, e13: PuzzleElement, e14: PuzzleElement, e15: PuzzleElement, e16: PuzzleElement, e17: PuzzleElement, e18: PuzzleElement, e19: PuzzleElement, e20: PuzzleElement, e21: PuzzleElement, e22: PuzzleElement, e23: PuzzleElement) {
    def |(e: PuzzleElement) = new Row24(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e17, e18, e19, e20, e21, e22, e23, e)
  }

  class Row24(e1: PuzzleElement, e2: PuzzleElement, e3: PuzzleElement, e4: PuzzleElement, e5: PuzzleElement, e6: PuzzleElement, e7: PuzzleElement, e8: PuzzleElement, e9: PuzzleElement, e10: PuzzleElement, e11: PuzzleElement, e12: PuzzleElement, e13: PuzzleElement, e14: PuzzleElement, e15: PuzzleElement, e16: PuzzleElement, e17: PuzzleElement, e18: PuzzleElement, e19: PuzzleElement, e20: PuzzleElement, e21: PuzzleElement, e22: PuzzleElement, e23: PuzzleElement, e24: PuzzleElement) {
    def |(e: PuzzleElement) = new Row(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e17, e18, e19, e20, e21, e22, e23, e24, e)
  }

  class Row(e1: PuzzleElement, e2: PuzzleElement, e3: PuzzleElement, e4: PuzzleElement, e5: PuzzleElement, e6: PuzzleElement, e7: PuzzleElement, e8: PuzzleElement, e9: PuzzleElement, e10: PuzzleElement, e11: PuzzleElement, e12: PuzzleElement, e13: PuzzleElement, e14: PuzzleElement, e15: PuzzleElement, e16: PuzzleElement, e17: PuzzleElement, e18: PuzzleElement, e19: PuzzleElement, e20: PuzzleElement, e21: PuzzleElement, e22: PuzzleElement, e23: PuzzleElement, e24: PuzzleElement, e25: PuzzleElement) {
    override def toString = s"$e1$e2$e3$e4$e5$e6$e7$e8$e9$e10$e11$e12$e13$e14$e15$e16$e17$e18$e19$e20$e21$e22$e23$e24$e25"

    def \\(r2: Row) = new Puzzle2(this, r2)

    lazy val elements: List[PuzzleElement] = List(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e17, e18, e19, e20, e21, e22, e23, e24, e25)

    lazy val rleForm: List[(PuzzleElement, Int)] = elements.foldRight[List[(PuzzleElement, Int)]](List.empty) {
      case (a, (b, x) :: t) if a != O && a == b => (a, x + 1) :: t
      case (O, (▢, x) :: t) => (▢, x + 1) :: t
      case (▢, (O, x) :: t) => (▢, x + 1) :: t
      case (O, l) => (▢, 1) :: l
      case (a, l) => (a, 1) :: l
    }
  }

  class Puzzle2(e1: Row, e2: Row) {
    def \\(e: Row) = new Puzzle3(e1, e2, e)
  }

  class Puzzle3(e1: Row, e2: Row, e3: Row) {
    def \\(e: Row) = new Puzzle4(e1, e2, e3, e)
  }

  class Puzzle4(e1: Row, e2: Row, e3: Row, e4: Row) {
    def \\(e: Row) = new Puzzle5(e1, e2, e3, e4, e)
  }

  class Puzzle5(e1: Row, e2: Row, e3: Row, e4: Row, e5: Row) {
    def \\(e: Row) = new Puzzle6(e1, e2, e3, e4, e5, e)
  }

  class Puzzle6(e1: Row, e2: Row, e3: Row, e4: Row, e5: Row, e6: Row) {
    def \\(e: Row) = new Puzzle7(e1, e2, e3, e4, e5, e6, e)
  }

  class Puzzle7(e1: Row, e2: Row, e3: Row, e4: Row, e5: Row, e6: Row, e7: Row) {
    def \\(e: Row) = new Puzzle8(e1, e2, e3, e4, e5, e6, e7, e)
  }

  class Puzzle8(e1: Row, e2: Row, e3: Row, e4: Row, e5: Row, e6: Row, e7: Row, e8: Row) {
    def \\(e: Row) = new Puzzle9(e1, e2, e3, e4, e5, e6, e7, e8, e)
  }

  class Puzzle9(e1: Row, e2: Row, e3: Row, e4: Row, e5: Row, e6: Row, e7: Row, e8: Row, e9: Row) {
    def \\(e: Row) = new Puzzle10(e1, e2, e3, e4, e5, e6, e7, e8, e9, e)
  }

  class Puzzle10(e1: Row, e2: Row, e3: Row, e4: Row, e5: Row, e6: Row, e7: Row, e8: Row, e9: Row, e10: Row) {
    def \\(e: Row) = new Puzzle11(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e)
  }

  class Puzzle11(e1: Row, e2: Row, e3: Row, e4: Row, e5: Row, e6: Row, e7: Row, e8: Row, e9: Row, e10: Row, e11: Row) {
    def \\(e: Row) = new Puzzle12(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e)
  }

  class Puzzle12(e1: Row, e2: Row, e3: Row, e4: Row, e5: Row, e6: Row, e7: Row, e8: Row, e9: Row, e10: Row, e11: Row, e12: Row) {
    def \\(e: Row) = new Puzzle13(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e)
  }

  class Puzzle13(e1: Row, e2: Row, e3: Row, e4: Row, e5: Row, e6: Row, e7: Row, e8: Row, e9: Row, e10: Row, e11: Row, e12: Row, e13: Row) {
    def \\(e: Row) = new Puzzle14(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e)
  }

  class Puzzle14(e1: Row, e2: Row, e3: Row, e4: Row, e5: Row, e6: Row, e7: Row, e8: Row, e9: Row, e10: Row, e11: Row, e12: Row, e13: Row, e14: Row) {
    def \\(e: Row) = new Puzzle15(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e)
  }

  class Puzzle15(e1: Row, e2: Row, e3: Row, e4: Row, e5: Row, e6: Row, e7: Row, e8: Row, e9: Row, e10: Row, e11: Row, e12: Row, e13: Row, e14: Row, e15: Row) {
    def \\(e: Row) = new Puzzle16(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e)
  }

  class Puzzle16(e1: Row, e2: Row, e3: Row, e4: Row, e5: Row, e6: Row, e7: Row, e8: Row, e9: Row, e10: Row, e11: Row, e12: Row, e13: Row, e14: Row, e15: Row, e16: Row) {
    def \\(e: Row) = new Puzzle17(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e)
  }

  class Puzzle17(e1: Row, e2: Row, e3: Row, e4: Row, e5: Row, e6: Row, e7: Row, e8: Row, e9: Row, e10: Row, e11: Row, e12: Row, e13: Row, e14: Row, e15: Row, e16: Row, e17: Row) {
    def \\(e: Row) = new Puzzle18(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e17, e)
  }

  class Puzzle18(e1: Row, e2: Row, e3: Row, e4: Row, e5: Row, e6: Row, e7: Row, e8: Row, e9: Row, e10: Row, e11: Row, e12: Row, e13: Row, e14: Row, e15: Row, e16: Row, e17: Row, e18: Row) {
    def \\(e: Row) = new Puzzle19(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e17, e18, e)
  }

  class Puzzle19(e1: Row, e2: Row, e3: Row, e4: Row, e5: Row, e6: Row, e7: Row, e8: Row, e9: Row, e10: Row, e11: Row, e12: Row, e13: Row, e14: Row, e15: Row, e16: Row, e17: Row, e18: Row, e19: Row) {
    def \\(e: Row) = new Puzzle20(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e17, e18, e19, e)
  }

  class Puzzle20(e1: Row, e2: Row, e3: Row, e4: Row, e5: Row, e6: Row, e7: Row, e8: Row, e9: Row, e10: Row, e11: Row, e12: Row, e13: Row, e14: Row, e15: Row, e16: Row, e17: Row, e18: Row, e19: Row, e20: Row) {
    def \\(e: Row) = new Puzzle21(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e17, e18, e19, e20, e)
  }

  class Puzzle21(e1: Row, e2: Row, e3: Row, e4: Row, e5: Row, e6: Row, e7: Row, e8: Row, e9: Row, e10: Row, e11: Row, e12: Row, e13: Row, e14: Row, e15: Row, e16: Row, e17: Row, e18: Row, e19: Row, e20: Row, e21: Row) {
    def \\(e: Row) = new Puzzle22(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e17, e18, e19, e20, e21, e)
  }

  class Puzzle22(e1: Row, e2: Row, e3: Row, e4: Row, e5: Row, e6: Row, e7: Row, e8: Row, e9: Row, e10: Row, e11: Row, e12: Row, e13: Row, e14: Row, e15: Row, e16: Row, e17: Row, e18: Row, e19: Row, e20: Row, e21: Row, e22: Row) {
    def \\(e: Row) = new Puzzle23(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e17, e18, e19, e20, e21, e22, e)
  }

  class Puzzle23(e1: Row, e2: Row, e3: Row, e4: Row, e5: Row, e6: Row, e7: Row, e8: Row, e9: Row, e10: Row, e11: Row, e12: Row, e13: Row, e14: Row, e15: Row, e16: Row, e17: Row, e18: Row, e19: Row, e20: Row, e21: Row, e22: Row, e23: Row) {
    def \\(e: Row) = new Puzzle24(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e17, e18, e19, e20, e21, e22, e23, e)
  }

  class Puzzle24(e1: Row, e2: Row, e3: Row, e4: Row, e5: Row, e6: Row, e7: Row, e8: Row, e9: Row, e10: Row, e11: Row, e12: Row, e13: Row, e14: Row, e15: Row, e16: Row, e17: Row, e18: Row, e19: Row, e20: Row, e21: Row, e22: Row, e23: Row, e24: Row) {
    def \\(e: Row) = new Puzzle(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e17, e18, e19, e20, e21, e22, e23, e24, e)
  }

  class Puzzle(e1: Row, e2: Row, e3: Row, e4: Row, e5: Row, e6: Row, e7: Row, e8: Row, e9: Row, e10: Row, e11: Row, e12: Row, e13: Row, e14: Row, e15: Row, e16: Row, e17: Row, e18: Row, e19: Row, e20: Row, e21: Row, e22: Row, e23: Row, e24: Row, e25: Row) {
    override def toString = s"$e1\n$e2\n$e3\n$e4\n$e5\n$e6\n$e7\n$e8\n$e9\n$e10\n$e11\n$e12\n$e13\n$e14\n$e15\n$e16\n$e17\n$e18\n$e19\n$e20\n$e21\n$e22\n$e23\n$e24\n$e25"

    lazy val rows: List[Row] = List(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e17, e18, e19, e20, e21, e22, e23, e24, e25)

    lazy val columns: List[Row] = {
      e1.elements zip e2.elements zip e3.elements zip e4.elements zip e5.elements zip e6.elements zip e7.elements zip e8.elements zip e9.elements zip e10.elements zip e11.elements zip e12.elements zip e13.elements zip e14.elements zip e15.elements zip e16.elements zip e17.elements zip e18.elements zip e19.elements zip e20.elements zip e21.elements zip e22.elements zip e23.elements zip e24.elements zip e25.elements map {
        case (((((((((((((((((((((((((c1), c2), c3), c4), c5), c6), c7), c8), c9), c10), c11), c12), c13), c14), c15), c16), c17), c18), c19), c20), c21), c22), c23), c24), c25) =>
          new Row(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21, c22, c23, c24, c25)
      }
    }
  }

  object Puzzle {
    def apply(vec: List[Row]): Puzzle = vec match {
      case list if list.size == 25 => new Puzzle(list(0), list(1), list(2), list(3), list(4), list(5), list(6), list(7), list(8), list(9), list(10), list(11), list(12), list(13), list(14), list(15), list(16), list(17), list(18), list(19), list(20), list(21), list(22), list(23), list(24))
    }

    def empty: Puzzle = apply(List.fill(25)(Row.empty))
  }

  object Row {
    def apply(vec: List[PuzzleElement]): Row = vec match {
      case list if list.size == 25 => new Row(list(0), list(1), list(2), list(3), list(4), list(5), list(6), list(7), list(8), list(9), list(10), list(11), list(12), list(13), list(14), list(15), list(16), list(17), list(18), list(19), list(20), list(21), list(22), list(23), list(24))
    }

    def empty: Row = apply(List.fill(25)(▢))
  }

  object ■ extends PuzzleElement {
    override def toString = "⬛ ️"
  }

  object ▢ extends PuzzleElement {
    override def toString = "⬜️ "
  }

  object O extends PuzzleElement {
    override def toString = "⬜️ "
  }

  val puzzle =
    (■ | ■ | ■ | ■ | ■ | ■ | ■ | O | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | O | ■ | ■ | ■ | ■ | ■ | ■ | ■) \\
      (■ | O | O | O | O | O | ■ | O | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | O | ■ | O | O | O | O | O | ■) \\
      (■ | O | ■ | ■ | ■ | O | ■ | O | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | O | ■ | O | ■ | ■ | ■ | O | ■) \\
      (■ | O | ■ | ■ | ■ | O | ■ | O | ▢ | ▢ | ▢ | ▢ | ■ | ■ | ▢ | ▢ | ▢ | O | ■ | O | ■ | ■ | ■ | O | ■) \\
      (■ | O | ■ | ■ | ■ | O | ■ | O | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | O | ■ | O | ■ | ■ | ■ | O | ■) \\
      (■ | O | O | O | O | O | ■ | O | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | O | ■ | O | O | O | O | O | ■) \\
      (■ | ■ | ■ | ■ | ■ | ■ | ■ | O | ■ | O | ■ | O | ■ | O | ■ | O | ■ | O | ■ | ■ | ■ | ■ | ■ | ■ | ■) \\
      (O | O | O | O | O | O | O | O | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | O | O | O | O | O | O | O | O) \\
      (▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ■ | ■ | ▢ | ▢ | ■ | ▢ | ▢ | ▢ | ■ | ■ | ▢ | ▢ | ■ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢) \\
      (▢ | ▢ | ▢ | ▢ | ▢ | ▢ | O | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢) \\
      (▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ■ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢) \\
      (▢ | ▢ | ▢ | ▢ | ▢ | ▢ | O | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢) \\
      (▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ■ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢) \\
      (▢ | ▢ | ▢ | ▢ | ▢ | ▢ | O | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢) \\
      (▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ■ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢) \\
      (▢ | ▢ | ▢ | ▢ | ▢ | ▢ | O | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢) \\
      (▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ■ | ▢ | ▢ | ▢ | ▢ | ■ | ▢ | ▢ | ▢ | ▢ | ■ | ▢ | ▢ | ▢ | ■ | ▢ | ▢ | ▢ | ▢) \\
      (O | O | O | O | O | O | O | O | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢) \\
      (■ | ■ | ■ | ■ | ■ | ■ | ■ | O | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢) \\
      (■ | O | O | O | O | O | ■ | O | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢) \\
      (■ | O | ■ | ■ | ■ | O | ■ | O | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢) \\
      (■ | O | ■ | ■ | ■ | O | ■ | O | ■ | ■ | ■ | O | ■ | ■ | ■ | ■ | ■ | ■ | ■ | ■ | ■ | ■ | O | ■ | ■) \\
      (■ | O | ■ | ■ | ■ | O | ■ | O | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢) \\
      (■ | O | O | O | O | O | ■ | O | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢) \\
      (■ | ■ | ■ | ■ | ■ | ■ | ■ | O | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢ | ▢)

  def couldYetBeValid(row: Row, constraints: ConstraintX): Boolean = {
    val cs = constraints.constraints
    val rowAsConstraints = row.rleForm.collect {
      case (■, x) => x
    }
    (row.rleForm.head match {
      case (■, x) => x <= cs.head
      case _ => true
    }) &&
      (row.rleForm.reverse.head match {
        case (■, x) => x <= cs.reverse.head
        case _ => true
      }) &&
      (rowAsConstraints.isEmpty || ((cs.max >= rowAsConstraints.max) &&
        (cs.sum >= rowAsConstraints.sum)))
  }

  def couldYetBeValid(puzzle: Puzzle, horizontals: ConstraintsX, verticals: ConstraintsX): Boolean = {
    require(horizontals.constraints.size == 25)
    require(verticals.constraints.size == 25)
    (puzzle.rows zip horizontals.constraints forall { case (a, b) => couldYetBeValid(a, b) }) &&
      (puzzle.columns zip verticals.constraints forall { case (a, b) => couldYetBeValid(a, b) })
  }

  def valid(row: Row, constraints: ConstraintX): Boolean = {
    val rowAsConstraints = row.rleForm.collect {
      case (■, x) => x
    }
    constraints.constraints == rowAsConstraints
  }

  def valid(puzzle: Puzzle, horizontals: ConstraintsX, verticals: ConstraintsX): Boolean = {
    require(horizontals.constraints.size == 25)
    require(verticals.constraints.size == 25)
    (puzzle.rows zip horizontals.constraints forall { case (a, b) => valid(a, b) }) &&
      (puzzle.columns zip verticals.constraints forall { case (a, b) => valid(a, b) })
  }

  def solve(puzzle: Puzzle, horizontals: ConstraintsX, verticals: ConstraintsX, guess: (Int, Int)): Option[Puzzle] = {
    if (valid(puzzle, horizontals, verticals))
      Some(puzzle)
    else
      guess match {
	case (row, column) =>
	  if (column == 0)
	    println(s"\n$puzzle")
	  val nextGuess: Puzzle => Option[(Int, Int)] = p => {
	    if ((column < 24) && !valid(p.rows(row), horizontals.constraints(row)) && couldYetBeValid(p.rows(row), horizontals.constraints(row)))
	      Some((row, column + 1))
	    else if ((row < 24) && valid(p.rows(row), horizontals.constraints(row)))
	      Some((row + 1, 0))
	    else
	      None
	  }
	  val r: Option[Puzzle] = puzzle.rows(row).elements(column) match {
	    case ▢ =>
	      val newRow = Row(puzzle.rows(row).elements.updated(column, ■))
	      val newPuzzle: Puzzle = Puzzle(puzzle.rows.updated(row, newRow))
	      if (valid(newPuzzle, horizontals, verticals)) {
		println(s"Solved!!!!1111\n$newPuzzle")
		Some(newPuzzle)
	      }
	      else if (couldYetBeValid(newPuzzle, horizontals, verticals)) {
		nextGuess(newPuzzle) flatMap (x => {
		  solve(newPuzzle, horizontals, verticals, x)
		})
	      }
	      else
		None
	    case _ => None
	  }
	  r match {
	    case None => nextGuess(puzzle) flatMap (x => solve(puzzle, horizontals, verticals, x))
	    case solution => solution
	  }
      }
  }

  def main(args: Array[String]) {
    println(puzzle)
    println("Solving…")
    solve(puzzle, ConstraintBuilding.horizontals, ConstraintBuilding.verticals, (0, 0)) match {
      case None => println("No Solution")
      case Some(solution) => println(solution)
    }
  }
}

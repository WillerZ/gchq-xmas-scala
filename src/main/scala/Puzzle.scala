
import scala.annotation.tailrec

case object Row {

  sealed trait RowEntry

  case object CertainlyFilled extends RowEntry {
    override def toString = "⬛️ "
  }

  case object CertainlyEmpty extends RowEntry {
    override def toString = "⬜️ "
  }

  case object UncertainContent extends RowEntry {
    override def toString = "__"
  }

  def allVariationsMeetingConstraints(constraints: List[Int]): Seq[Row] = {
    def helper(cs: List[Int], length: Int): Seq[Row] = cs match {
      case Nil => Seq(Row(Seq.fill(length)(Row.CertainlyEmpty)))
      case h::Nil =>
        val slack = length - h
        val filled = Seq.fill(h)(Row.CertainlyFilled)
        val empties = (0 to slack).map {a => Seq.fill(a)(Row.CertainlyEmpty)}
        empties.reverse zip empties map { case (before, after) => Row(before ++ filled ++ after) }
      case h::t =>
        val maxBefore = length - (cs.sum + cs.size - 1)
        val emptyAfter = Seq(Row.CertainlyEmpty)
        val filled = Seq.fill(h)(Row.CertainlyFilled)
        (0 to maxBefore).flatMap {
          before => val emptyBefore = Seq.fill(before)(Row.CertainlyEmpty)
            helper(t, length - before - h - 1) map (x => Row(emptyBefore ++ filled ++ emptyAfter ++ x.entries))
        }
    }
    helper(constraints, 25)
  }
}

case class Row(entries: Seq[Row.RowEntry] = Seq.empty) {

  import Row._

  def ■ = Row(entries :+ CertainlyFilled)

  def ▢ = Row(entries :+ CertainlyEmpty)

  def + = Row(entries :+ UncertainContent)

  def canBecome(other: Row): Boolean = {
    entries zip other.entries forall {
      case (UncertainContent, _) => true
      case (a, b) if a == b => true
      case _ => false
    }
  }

  override def toString = {
    entries.foldLeft[String]("")({ case (accum, entry) => accum + entry.toString })
  }
}

class RowValidator(validRows: Seq[Row]) {
  final def isValid(row: Row): Boolean = validRows.contains(row)

  final def couldBeValid(row: Row): Boolean = validVariations(row).nonEmpty

  final def validVariations(row: Row): Seq[Row] = validRows.collect {
    case a if row.canBecome(a) => a
  }

  final def varietyCount: Int = validRows.size
}


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

  def asConstraints(row: Int): List[Int] = {
    @tailrec
    def helper(row: Int, idx: Int, accumulatedBits: Int, sofar: List[Int]): List[Int] = if (idx > 24) {
      if (accumulatedBits > 0)
        accumulatedBits :: sofar
      else
        sofar
    }
    else {
      row & 1 match {
        case 1 => helper(row >> 1, idx + 1, accumulatedBits + 1, sofar)
        case 0 if accumulatedBits > 0 => helper(row >> 1, idx + 1, 0, accumulatedBits :: sofar)
        case _ => helper(row >> 1, idx + 1, 0, sofar)
      }
    }
    helper(row, 0, 0, List.empty[Int])
  }

  final val (horizontals, verticals): (List[RowValidator], List[RowValidator]) = {
    import Row.{CertainlyFilled, CertainlyEmpty, RowEntry}

    val hc = ((7 ~ 3 ~ 1 ~ 1 ~ 7) \\
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
      (7 ~ 2 ~ 1 ~ 2 ~ 5)).constraints map (_.constraints)
    val vc = ((7 ~ 2 ~ 1 ~ 1 ~ 7) \\
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
      (7 ~ 1 ~ 3 ~ 2 ~ 1 ~ 1)).constraints map (_.constraints)
    val allv = (hc ++ vc).distinct.map(a => a -> new RowValidator(scala.util.Random.shuffle(Row.allVariationsMeetingConstraints(a)))).toMap
    (hc map allv, vc map allv)
  }
}

case object PuzzleBuilder {

  implicit class Puzzle1(e1: Row) {
    def \\(e: Row) = new Puzzle2(e1, e)
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
      val l = (0 to 24).map(column => {
        Row(rows.map({
          case row => row.entries(column)
        }))
      }).toList
      require(l.size == 25)
      l
    }

    def flipped: Puzzle = Puzzle(columns)
  }

  object Puzzle {
    def apply(vec: List[Row]): Puzzle = vec match {
      case list if list.size == 25 => new Puzzle(list(0), list(1), list(2), list(3), list(4), list(5), list(6), list(7), list(8), list(9), list(10), list(11), list(12), list(13), list(14), list(15), list(16), list(17), list(18), list(19), list(20), list(21), list(22), list(23), list(24))
    }

    def empty: Puzzle = apply(List.fill(25)(Row(Seq.fill(25)(Row.UncertainContent))))
  }

  val R = Row()

  val puzzle = Puzzle.empty
//      R.■.■.■.■.■.■.■.▢.+.+.+.+.+.+.+.+.+.▢.■.■.■.■.■.■.■ \\
//      R.■.▢.▢.▢.▢.▢.■.▢.+.+.+.+.+.+.+.+.+.▢.■.▢.▢.▢.▢.▢.■ \\
//      R.■.▢.■.■.■.▢.■.▢.+.+.+.+.+.+.+.+.+.▢.■.▢.■.■.■.▢.■ \\
//      R.■.▢.■.■.■.▢.■.▢.+.+.+.+.+.+.+.+.+.▢.■.▢.■.■.■.▢.■ \\
//      R.■.▢.■.■.■.▢.■.▢.+.+.+.+.+.+.+.+.+.▢.■.▢.■.■.■.▢.■ \\
//      R.■.▢.▢.▢.▢.▢.■.▢.+.+.+.+.+.+.+.+.+.▢.■.▢.▢.▢.▢.▢.■ \\
//      R.■.■.■.■.■.■.■.▢.■.▢.■.▢.■.▢.■.▢.■.▢.■.■.■.■.■.■.■ \\
//      R.▢.▢.▢.▢.▢.▢.▢.▢.+.+.+.+.+.+.+.+.+.▢.▢.▢.▢.▢.▢.▢.▢ \\
//      R.+.+.+.+.+.+.■.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+ \\
//      R.+.+.+.+.+.+.▢.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+ \\
//      R.+.+.+.+.+.+.■.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+ \\
//      R.+.+.+.+.+.+.▢.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+ \\
//      R.+.+.+.+.+.+.■.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+ \\
//      R.+.+.+.+.+.+.▢.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+ \\
//      R.+.+.+.+.+.+.■.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+ \\
//      R.+.+.+.+.+.+.▢.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+ \\
//      R.+.+.+.+.+.+.■.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+ \\
//      R.▢.▢.▢.▢.▢.▢.▢.▢.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+ \\
//      R.■.■.■.■.■.■.■.▢.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+ \\
//      R.■.▢.▢.▢.▢.▢.■.▢.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+ \\
//      R.■.▢.■.■.■.▢.■.▢.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+ \\
//      R.■.▢.■.■.■.▢.■.▢.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+ \\
//      R.■.▢.■.■.■.▢.■.▢.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+ \\
//      R.■.▢.▢.▢.▢.▢.■.▢.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+ \\
//      R.■.■.■.■.■.■.■.▢.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+.+

  def couldYetBeValid(puzzle: Puzzle, horizontals: List[RowValidator], verticals: List[RowValidator]): Boolean = {
    require(horizontals.size == 25)
    require(verticals.size == 25)
    (puzzle.rows zip horizontals forall { case (a, b) => b.couldBeValid(a) }) &&
      (puzzle.columns zip verticals forall { case (a, b) => b.couldBeValid(a) })
  }

  def valid(puzzle: Puzzle, horizontals: List[RowValidator], verticals: List[RowValidator]): Boolean = {
    require(horizontals.size == 25)
    require(verticals.size == 25)
    (puzzle.rows zip horizontals forall { case (a, b) => b.isValid(a) }) &&
      (puzzle.columns zip verticals forall { case (a, b) => b.isValid(a) })
  }

  def solve(puzzle: Puzzle, horizontals: List[RowValidator], verticals: List[RowValidator], rowIndexesToPermute: Set[Int], columnIndexesToPermute: Set[Int]): Option[Puzzle] = {
    if (valid(puzzle, horizontals, verticals))
      Some(puzzle)
    else if (!couldYetBeValid(puzzle, horizontals, verticals))
      None
    else {
      val bestRowAndScore = rowIndexesToPermute.map(idx => idx -> horizontals(idx).validVariations(puzzle.rows(idx)).size).toList.sortBy(_._2).head
      val bestColAndScore = columnIndexesToPermute.map(idx => idx -> verticals(idx).validVariations(puzzle.columns(idx)).size).toList.sortBy(_._2).head
      val (puz, rowVs, colVs, rowIs, colIs, replaceRow, transform): (Puzzle, List[RowValidator], List[RowValidator], Set[Int], Set[Int], Int, Puzzle => Puzzle) =
        if (bestRowAndScore._2 <= bestColAndScore._2)
          (puzzle, horizontals, verticals, rowIndexesToPermute, columnIndexesToPermute, bestRowAndScore._1, identity)
        else
          (puzzle.flipped, verticals, horizontals, columnIndexesToPermute, rowIndexesToPermute, bestColAndScore._1, { a: Puzzle => a.flipped })
      rowVs(replaceRow).validVariations(puz.rows(replaceRow)).foldLeft[Option[Puzzle]](None)({
        case (None, candidate) => solve(Puzzle(puz.rows.updated(replaceRow, candidate)), rowVs, colVs, rowIs - replaceRow, colIs)
        case (solution, _) => solution
      }).map(transform)
    }
  }

  def main(args: Array[String]) {
    val rowIndexSet = ConstraintBuilding.horizontals.indices.toSet
    val colIndexSet = ConstraintBuilding.verticals.indices.toSet
    solve(puzzle, ConstraintBuilding.horizontals, ConstraintBuilding.verticals, rowIndexSet, colIndexSet) match {
      case None => println("No Solution")
      case Some(solution) => println(s"Solution is\n$solution")
    }
  }
}

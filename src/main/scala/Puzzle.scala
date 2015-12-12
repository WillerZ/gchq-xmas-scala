import scala.annotation.tailrec

case class Row(val bitmap: Int = 0) {
  def ■ = new Row((bitmap << 1) | 1)

  def ▢ = new Row(bitmap << 1)

  override def toString = {
    @tailrec
    def helper(row: Int, idx: Int, accum: String): String = if (idx > 24)
      accum
    else
      helper(row >> 1, idx + 1, {if ((row & 0x1) == 0x1) "■ " else "▢ "} + accum)
    helper(bitmap, 0, "")
  }
}

class RowValidator(validRows: List[Int]) {
  final def isValid(row: Row): Boolean = validRows.contains(row)

  final def couldBeValid(row: Row): Boolean = validVariations(row).nonEmpty

  final def validVariations(row: Row): List[Row] = validRows.collect {
    case a if row.bitmap.&(~a) == 0 => Row(a)
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
    def validators: List[RowValidator] = constraints map (a => new RowValidator(a.constraints))

    def \\(constraintX: ConstraintX) = new ConstraintsX(constraints :+ constraintX)
  }

  def asConstraints(row: Int): List[Int] = {
    @tailrec
    def helper(row: Int, idx: Int, accumulatedBits: Int, sofar: List[Int]): List[Int] = if (idx > 24)
      {
        if (accumulatedBits > 0)
          accumulatedBits :: sofar
        else
          sofar
      }
    else
      {
        (row & 1) match {
          case 1 => helper(row >> 1, idx+1, accumulatedBits + 1, sofar)
          case 0 if accumulatedBits > 0 => helper(row >> 1, idx+1, 0, accumulatedBits :: sofar)
          case _ => helper(row >> 1, idx+1, 0, sofar)
        }
      }
    helper(row, 0, 0, List.empty[Int])
  }

  class RowValidatorBuilding {
    var validRows = scala.collection.mutable.ListBuffer.empty[Int]
    def addValidRow(row: Int): Unit = validRows += row

    def finish: RowValidator = new RowValidator(validRows.toList)
  }

  final val (horizontals, verticals): (List[RowValidator],List[RowValidator]) = {
    val allPossibleRows = List.range[Int](0, 0x1ffffff)

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
    val allc = (hc ++ vc).distinct.map(a => a->new RowValidatorBuilding ).toMap
    allPossibleRows.foreach( row => {
      if (row.&(0xffff) == 0)
        println(s"${row*100/0x1ffffff}% initialised")
      val cf = asConstraints(row)
      if (allc.isDefinedAt(cf))
        allc(cf).addValidRow(row)
    } )
    val allv = allc.mapValues( _.finish )
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
      List.range(0, 24).map(column => {
        val mask: Int = 1 << column
        Row(rows map(row => row.bitmap & mask) reduce[Int] { case (a, b) =>a | b })
      }).reverse
    }
  }

  object Puzzle {
    def apply(vec: List[Row]): Puzzle = vec match {
      case list if list.size == 25 => new Puzzle(list(0), list(1), list(2), list(3), list(4), list(5), list(6), list(7), list(8), list(9), list(10), list(11), list(12), list(13), list(14), list(15), list(16), list(17), list(18), list(19), list(20), list(21), list(22), list(23), list(24))
    }

    def empty: Puzzle = apply(List.fill(25)(Row()))
  }

  val R = Row()

  val puzzle =
      R.■.■.■.■.■.■.■.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.■.■.■.■.■.■.■ \\
      R.■.▢.▢.▢.▢.▢.■.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.■.▢.▢.▢.▢.▢.■ \\
      R.■.▢.■.■.■.▢.■.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.■.▢.■.■.■.▢.■ \\
      R.■.▢.■.■.■.▢.■.▢.▢.▢.▢.▢.■.■.▢.▢.▢.▢.■.▢.■.■.■.▢.■ \\
      R.■.▢.■.■.■.▢.■.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.■.▢.■.■.■.▢.■ \\
      R.■.▢.▢.▢.▢.▢.■.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.■.▢.▢.▢.▢.▢.■ \\
      R.■.■.■.■.■.■.■.▢.■.▢.■.▢.■.▢.■.▢.■.▢.■.■.■.■.■.■.■ \\
      R.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢ \\
      R.▢.▢.▢.▢.▢.▢.■.■.▢.▢.■.▢.▢.▢.■.■.▢.▢.■.▢.▢.▢.▢.▢.▢ \\
      R.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢ \\
      R.▢.▢.▢.▢.▢.▢.■.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢ \\
      R.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢ \\
      R.▢.▢.▢.▢.▢.▢.■.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢ \\
      R.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢ \\
      R.▢.▢.▢.▢.▢.▢.■.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢ \\
      R.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢ \\
      R.▢.▢.▢.▢.▢.▢.■.▢.▢.▢.▢.■.▢.▢.▢.▢.■.▢.▢.▢.■.▢.▢.▢.▢ \\
      R.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢ \\
      R.■.■.■.■.■.■.■.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢ \\
      R.■.▢.▢.▢.▢.▢.■.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢ \\
      R.■.▢.■.■.■.▢.■.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢ \\
      R.■.▢.■.■.■.▢.■.▢.■.■.■.▢.■.■.■.■.■.■.■.■.■.■.▢.■.■ \\
      R.■.▢.■.■.■.▢.■.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢ \\
      R.■.▢.▢.▢.▢.▢.■.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢ \\
      R.■.■.■.■.■.■.■.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢.▢

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

  def solve(puzzle: Puzzle, horizontals: List[RowValidator], verticals: List[RowValidator], rowIndexesToPermute: List[Int]): Option[Puzzle] = {
    if (valid(puzzle, horizontals, verticals))
      Some(puzzle)
    else if (!couldYetBeValid(puzzle, horizontals, verticals))
      None
    else
      rowIndexesToPermute match {
        case h :: t => horizontals(h).validVariations(puzzle.rows(h)).foldLeft[Option[Puzzle]](None) {
          case (None, candidate) => solve(Puzzle(puzzle.rows.updated(h, candidate)), horizontals, verticals, t)
          case (solution, _) => solution
        }
        case _ => None
      }
  }

  def main(args: Array[String]) {
    println(puzzle)
    val horizontalVariations = ConstraintBuilding.horizontals.foldLeft[Double](1.0) {case (a, b) => a * b.varietyCount}
    println(s"There are $horizontalVariations variations horizontally")
    val verticalVariations = ConstraintBuilding.verticals.foldLeft[Double](1.0) {case (a, b) => a * b.varietyCount}
    println(s"There are $verticalVariations variations vertically")
    val flip: Puzzle => Puzzle = puzzle => Puzzle(puzzle.columns)
    val (solveThis, rowConstraints, colConstraints, transform): (Puzzle, List[RowValidator], List[RowValidator], Puzzle => Puzzle) = if (verticalVariations >= horizontalVariations)
      (puzzle, ConstraintBuilding.horizontals, ConstraintBuilding.verticals, identity)
    else
      (flip(puzzle), ConstraintBuilding.verticals, ConstraintBuilding.horizontals, flip)
    val rowOrder = rowConstraints.zipWithIndex.sortBy(a => a._1.varietyCount).map(a=> a._2)
    println("Row order is $rowOrder")
    println(s"Solving\n$solveThis")
    solve(puzzle, rowConstraints, colConstraints, rowOrder) match {
      case None => println("No Solution")
      case Some(solution) => println(s"Solution is\n${transform(solution)}")
    }
  }
}

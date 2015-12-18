
import scala.annotation.tailrec

case object RowBuilding {

  sealed trait RowEntry {
    def ■(): Row = new Row(Seq(this, RowBuilding.■))

    def □(): Row = new Row(Seq(this, RowBuilding.□))

    def ■(e: RowEntry): Row = new Row(Seq(this, RowBuilding.■, e))

    def □(e: RowEntry): Row = new Row(Seq(this, RowBuilding.□, e))
  }

  case object ■ extends RowEntry {
    override def toString = "⬛️ "
  }

  case object CertainlyEmpty extends RowEntry {
    override def toString = "⬜️ "
  }

  case object □ extends RowEntry {
    override def toString = "__"
  }

  implicit class Row1(entry: RowEntry) extends Row(Seq(entry))

  class Row(val entries: Seq[RowEntry]) {
    def ■(): Row = new Row(entries :+ RowBuilding.■)

    def □(): Row = new Row(entries :+ RowBuilding.□)

    def ■(e: RowEntry): Row = new Row(entries :+ RowBuilding.■ :+ e)

    def □(e: RowEntry): Row = new Row(entries :+ RowBuilding.□ :+ e)

    def ::(c: ConstraintBuilding.Constraint) = new ConstrainedRow(entries, c)

    def canBecome(other: Row): Boolean = {
      require(entries.size == other.entries.size, "Rows cannot change size")
      entries zip other.entries forall {
        case (RowBuilding.□, _) => true
        case (a, b) if a == b => true
        case _ => false
      }
    }

    override def toString = entries mkString ""

    override def equals(other: Any) = other match {
      case row: Row => entries == row.entries
      case _ => false
    }
  }

  class ConstrainedRow(entries: Seq[RowEntry], val constraints: ConstraintBuilding.Constraint) extends Row(entries)

  def allVariationsMeetingConstraints(constraints: List[Int]): Seq[Row] = {
    def helper(cs: List[Int], length: Int): Seq[Row] = cs match {
      case Nil => Seq(new Row(Seq.fill(length)(CertainlyEmpty)))
      case h :: Nil =>
        val slack = length - h
        val filled = Seq.fill(h)(■)
        val empties = (0 to slack).map { a => Seq.fill(a)(CertainlyEmpty) }
        empties.reverse zip empties map { case (before, after) => new Row(before ++ filled ++ after) }
      case h :: t =>
        val maxBefore = length - (cs.sum + cs.size - 1)
        val emptyAfter = Seq(CertainlyEmpty)
        val filled = Seq.fill(h)(■)
        (0 to maxBefore).flatMap {
          before => val emptyBefore = Seq.fill(before)(CertainlyEmpty)
            helper(t, length - before - h - 1) map (x => new Row(emptyBefore ++ filled ++ emptyAfter ++ x.entries))
        }
    }
    helper(constraints, 25)
  }

  class RowValidator(validRows: Seq[Row]) {
    final def isValid(row: Row): Boolean = validRows.contains(row)

    final def couldBeValid(row: Row): Boolean = validVariations(row).nonEmpty

    final def validVariations(row: Row): Seq[Row] = validRows.collect {
      case a if row.canBecome(a) => a
    }

    final def varietyCount: Int = validRows.size

    final def certainties(row: Row): Row = {
      val reduceVariationsToCommonality: (Row, Row) => Row = {
        case (a, b) => new Row(a.entries zip b.entries map {
          case (c, d) if c == d => c
          case _ => □
        })
      }
      validVariations(row).reduce(reduceVariationsToCommonality)
    }
  }

}

case object ConstraintBuilding {

  implicit class Constraint1(c1: Int) extends Constraint(List(c1))

  class Constraint(val constraints: List[Int]) {
    def ~(c: Int) = new Constraint(constraints :+ c)

    def \\(constraint: Constraint) = new ConstraintsX(List(this, constraint))
  }

  class ConstraintsX(val constraints: List[Constraint]) {
    def \\(constraintX: Constraint) = new ConstraintsX(constraints :+ constraintX)
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

  import RowBuilding._

  final val (horizontals, verticals): (List[RowValidator], List[RowValidator]) = {
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
    val allv = (hc ++ vc).distinct.map(a => a -> new RowValidator(scala.util.Random.shuffle(allVariationsMeetingConstraints(a)))).toMap
    (hc map allv, vc map allv)
  }
}

case object PuzzleBuilding {

  import RowBuilding._

  implicit class Puzzle1(e1: Row) extends Puzzle(Seq(e1))

  class Puzzle(val rows: Seq[Row]) {
    require(rows.nonEmpty, "Puzzles must have at least one row")
    require(rows.map(_.entries.size).distinct.size == 1, "Puzzles must be rectangular")

    val columns: Seq[Row] = {
      val l = rows.head.entries.indices.map(column => {
        new Row(rows.map({
          case row => row.entries(column)
        }))
      }).toSeq
      l
    }

    def &(e: Row) = new Puzzle(rows :+ e)

    override def toString = rows mkString "\n"

    def flipped: Puzzle = new Puzzle(columns)
  }

  object Puzzle {
    def empty(size: Int): Puzzle = new Puzzle(List.fill(size)(new Row(Seq.fill(size)(□))))

    def empty(rows: Int, columns: Int): Puzzle = new Puzzle(List.fill(rows)(new Row(Seq.fill(columns)(□))))
  }

  def couldYetBeValid(puzzle: Puzzle, horizontals: List[RowValidator], verticals: List[RowValidator]): Boolean = {
    (puzzle.rows zip horizontals forall { case (a, b) => b.couldBeValid(a) }) &&
      (puzzle.columns zip verticals forall { case (a, b) => b.couldBeValid(a) })
  }

  def valid(puzzle: Puzzle, horizontals: List[RowValidator], verticals: List[RowValidator]): Boolean = {
    require(horizontals.size == puzzle.rows.size)
    require(verticals.size == puzzle.columns.size)
    (puzzle.rows zip horizontals forall { case (a, b) => b.isValid(a) }) &&
      (puzzle.columns zip verticals forall { case (a, b) => b.isValid(a) })
  }

  def solve(puzzleIn: Puzzle, horizontals: List[RowValidator], verticals: List[RowValidator], rowIndexesToPermute: Set[Int], columnIndexesToPermute: Set[Int]): Option[Puzzle] = {
    println(puzzleIn)
    require(puzzleIn.rows.size == horizontals.size, "Must have as many row constraints as rows")
    require(puzzleIn.columns.size == verticals.size, "Must have as many column constraints as columns")
    if (valid(puzzleIn, horizontals, verticals))
      Some(puzzleIn)
    else if (!couldYetBeValid(puzzleIn, horizontals, verticals)) {
      None
    }
    else {
      @tailrec
      def fillInWhatWeKnow(puzzle: Puzzle): Puzzle = {
        val inRows = puzzle.rows
        val outPuzzle = new Puzzle(new Puzzle(puzzle.rows zip horizontals map {
          case (row, validator) => validator certainties row
        }).columns zip verticals map {
          case (row, validator) => validator certainties row
        }).flipped
        if (outPuzzle.rows == inRows)
          outPuzzle
        else
          fillInWhatWeKnow(outPuzzle)
      }
      val puzzle = fillInWhatWeKnow(puzzleIn)
      println(puzzle)
      val bestRowAndScore = rowIndexesToPermute.map(idx => idx -> horizontals(idx).validVariations(puzzle.rows(idx)).size).toList.sortBy(_._2).head
      val bestColAndScore = columnIndexesToPermute.map(idx => idx -> verticals(idx).validVariations(puzzle.columns(idx)).size).toList.sortBy(_._2).head
      val (puz, rowVs, colVs, rowIs, colIs, replaceRow, transform): (Puzzle, List[RowValidator], List[RowValidator], Set[Int], Set[Int], Int, Puzzle => Puzzle) =
        if (bestRowAndScore._2 <= bestColAndScore._2)
          (puzzle, horizontals, verticals, rowIndexesToPermute, columnIndexesToPermute, bestRowAndScore._1, identity)
        else
          (puzzle.flipped, verticals, horizontals, columnIndexesToPermute, rowIndexesToPermute, bestColAndScore._1, { a: Puzzle => a.flipped })
      rowVs(replaceRow).validVariations(puz.rows(replaceRow)).foldLeft[Option[Puzzle]](None)({
        case (None, candidate) => solve(new Puzzle(puz.rows.updated(replaceRow, candidate)), rowVs, colVs, rowIs - replaceRow, colIs)
        case (solution, _) => solution
      }).map(transform)
    }
  }

}

object Solve {

  import RowBuilding._

  import PuzzleBuilding._

  import ConstraintBuilding._

  // @formatter:off
  val puzzle =
                7 ~ 3 ~ 1 ~ 1 ~ 7 :: □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ &
            1 ~ 1 ~ 2 ~ 2 ~ 1 ~ 1 :: □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ &
    1 ~ 3 ~ 1 ~ 3 ~ 1 ~ 1 ~ 3 ~ 1 :: □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ &
    1 ~ 3 ~ 1 ~ 1 ~ 6 ~ 1 ~ 3 ~ 1 :: □ □ □ ■ ■ □ □ □ □ □ □ □ ■ ■ □ □ □ □ □ □ □ ■ □ □ □ &
    1 ~ 3 ~ 1 ~ 5 ~ 2 ~ 1 ~ 3 ~ 1 :: □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ &
                1 ~ 1 ~ 2 ~ 1 ~ 1 :: □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ &
        7 ~ 1 ~ 1 ~ 1 ~ 1 ~ 1 ~ 7 :: □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ &
                            3 ~ 3 :: □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ &
1 ~ 2 ~ 3 ~ 1 ~ 1 ~ 3 ~ 1 ~ 1 ~ 2 :: □ □ □ □ □ □ ■ ■ □ □ ■ □ □ □ ■ ■ □ □ ■ □ □ □ □ □ □ &
            1 ~ 1 ~ 3 ~ 2 ~ 1 ~ 1 :: □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ &
            4 ~ 1 ~ 4 ~ 2 ~ 1 ~ 2 :: □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ &
    1 ~ 1 ~ 1 ~ 1 ~ 1 ~ 4 ~ 1 ~ 3 :: □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ &
            2 ~ 1 ~ 1 ~ 1 ~ 2 ~ 5 :: □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ &
            3 ~ 2 ~ 2 ~ 6 ~ 3 ~ 1 :: □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ &
            1 ~ 9 ~ 1 ~ 1 ~ 2 ~ 1 :: □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ &
            2 ~ 1 ~ 2 ~ 2 ~ 3 ~ 1 :: □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ &
        3 ~ 1 ~ 1 ~ 1 ~ 1 ~ 5 ~ 1 :: □ □ □ □ □ □ ■ □ □ □ □ ■ □ □ □ □ ■ □ □ □ ■ □ □ □ □ &
                    1 ~ 2 ~ 2 ~ 5 :: □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ &
	7 ~ 1 ~ 2 ~ 1 ~ 1 ~ 1 ~ 3 :: □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ &
	1 ~ 1 ~ 2 ~ 1 ~ 2 ~ 2 ~ 1 :: □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ &
	    1 ~ 3 ~ 1 ~ 4 ~ 5 ~ 1 :: □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ &
	   1 ~ 3 ~ 1 ~ 3 ~ 10 ~ 2 :: □ □ □ ■ ■ □ □ □ □ ■ ■ □ □ □ □ ■ □ □ □ □ ■ ■ □ □ □ &
	    1 ~ 3 ~ 1 ~ 1 ~ 6 ~ 6 :: □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ &
	    1 ~ 1 ~ 2 ~ 1 ~ 1 ~ 2 :: □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ &
		7 ~ 2 ~ 1 ~ 2 ~ 5 :: □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □ □
  // @formatter:on

  def main(args: Array[String]) {
    val rowIndexSet = ConstraintBuilding.horizontals.indices.toSet
    val colIndexSet = ConstraintBuilding.verticals.indices.toSet
    solve(puzzle, ConstraintBuilding.horizontals, ConstraintBuilding.verticals, rowIndexSet, colIndexSet) match {
      case None => println("No Solution")
      case Some(solution) => println(s"Solution is\n$solution")
    }
  }
}

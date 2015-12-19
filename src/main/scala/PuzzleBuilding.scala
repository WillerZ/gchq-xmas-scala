/**
  * Created by philwill on 19/12/2015.
  */
case object PuzzleBuilding {

  sealed trait RowEntry {
    def ■(): Row = new Row(Seq(this, PuzzleBuilding.■))

    def □(): Row = new Row(Seq(this, PuzzleBuilding.□))

    def ■(e: RowEntry): Row = new Row(Seq(this, PuzzleBuilding.■, e))

    def □(e: RowEntry): Row = new Row(Seq(this, PuzzleBuilding.□, e))
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
    def ■(): Row = new Row(entries :+ PuzzleBuilding.■)

    def □(): Row = new Row(entries :+ PuzzleBuilding.□)

    def ■(e: RowEntry): Row = new Row(entries :+ PuzzleBuilding.■ :+ e)

    def □(e: RowEntry): Row = new Row(entries :+ PuzzleBuilding.□ :+ e)

    def ::(c: Constraint) = new ConstrainedRow(entries, new RowValidator(scala.util.Random.shuffle(allVariationsMeetingConstraints(c.constraints,entries.size))))

    def canBecome(other: Row): Boolean = {
      require(entries.size == other.entries.size, "Rows cannot change size")
      entries zip other.entries forall {
        case (PuzzleBuilding.□, _) => true
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

  class ConstrainedRow(entries: Seq[RowEntry], val validator: RowValidator) extends Row(entries) {
    def couldBecomeValid: Boolean = validator.couldBecomeValid(this)
    def isValid: Boolean = validator.isValid(this)
    def withCertainties: ConstrainedRow = new ConstrainedRow(validator.certainties(this).entries, validator)
    def validVariations: Seq[ConstrainedRow] = validator.validVariations(this).map(r => new ConstrainedRow(r.entries, validator))
    def varietyCount: Int = validator.validVariations(this).size
  }

  def allVariationsMeetingConstraints(constraints: List[Int], length: Int): Seq[Row] = {
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
    helper(constraints, length)
  }

  class RowValidator(validRows: Seq[Row]) {
    final def isValid(row: Row): Boolean = validRows.contains(row)

    final def couldBecomeValid(row: Row): Boolean = validVariations(row).nonEmpty

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

  implicit class Constraint1(c1: Int) extends Constraint(List(c1))

  class Constraint(val constraints: List[Int]) {
    def ~(c: Int) = new Constraint(constraints :+ c)
  }

  implicit class Puzzle1(e1: ConstrainedRow) extends PartialPuzzle(Seq(e1))

  implicit class VerticalConstraintsAttractor(vcString: String) {

    def vc(partialPuzzle: PartialPuzzle) = {
      val constraints = {
        val length = partialPuzzle.rows.head.entries.length
        val tbd = vcString.stripMargin.split('\n').tail.map(_.split('|').take(length).map({
          case " " => None
          case x => Some(x.toInt)
        }))
        val tbd2 = partialPuzzle.rows.head.entries.indices.map(column => tbd.collect({
            case x if x(column).isDefined => x(column).get
          }).toList)
        val tbd3 = tbd2.map(a => new RowValidator(scala.util.Random.shuffle(allVariationsMeetingConstraints(a,length))))
        tbd3
      }
      new Puzzle(partialPuzzle.rows, constraints)
    }

  }

  class PartialPuzzle(val rows: Seq[ConstrainedRow]) {
    require(rows.nonEmpty, "Puzzles must have at least one row")
    require(rows.map(_.entries.size).distinct.size == 1, "Puzzles must be rectangular")

    def &(e: ConstrainedRow) = new PartialPuzzle(rows :+ e)

    override def toString = rows mkString "\n"
  }

  class Puzzle(rows: Seq[ConstrainedRow], val columnConstraints: Seq[RowValidator]) extends PartialPuzzle(rows) {
    require(rows.head.entries.size == columnConstraints.size, "Every column must have constraints")

    val columns: Seq[ConstrainedRow] = {
      val l = rows.head.entries.indices.map(column => {
        new Row(rows.map({
          case row => row.entries(column)
        }))
      }).toSeq
      l zip columnConstraints map (a => new ConstrainedRow(a._1.entries, a._2))
    }

    def flipped: Puzzle = new Puzzle(columns, rows map (_.validator))

    def couldBecomeValid: Boolean = rows.forall(_.couldBecomeValid) && columns.forall(_.couldBecomeValid)

    def isValid: Boolean = rows.forall(_.isValid) && columns.forall(_.isValid)

    private def withKnownEntriesFilled: Puzzle = {
      @scala.annotation.tailrec
      def fillInWhatWeKnow(puzzle: Puzzle): Puzzle = {
        val rowsDonePuzzle = new Puzzle(puzzle.rows map {
          case row => row.withCertainties
        }, puzzle.columnConstraints).flipped
        val outPuzzle = new Puzzle(rowsDonePuzzle.rows map {
          case row => row.withCertainties
        }, rowsDonePuzzle.columnConstraints).flipped
        if (outPuzzle.rows == puzzle.rows)
          outPuzzle
        else
          fillInWhatWeKnow(outPuzzle)
      }
      fillInWhatWeKnow(this)
    }

    def solution: Option[Puzzle] = {
      if (isValid)
        Some(this)
      else if (!couldBecomeValid) {
        None
      }
      else {
        val candidate = withKnownEntriesFilled
        if (candidate.isValid)
          Some(candidate)
        else {
          val bestRowAndScore = candidate.rows.zipWithIndex.map({case (row,idx) => idx -> row.varietyCount}).sortBy(_._2).head
          val bestColAndScore = candidate.columns.zipWithIndex.map({case (row,idx) => idx -> row.varietyCount}).sortBy(_._2).head
          val (puz, replaceRow, transform): (Puzzle, Int, Puzzle => Puzzle) =
            if (bestRowAndScore._2 <= bestColAndScore._2)
              (candidate, bestRowAndScore._1, identity)
            else
              (candidate.flipped, bestColAndScore._1, { a: Puzzle => a.flipped })
          puz.rows(replaceRow).validVariations.foldLeft[Option[Puzzle]](None)({
            case (None, tryThis) => new Puzzle(puz.rows.updated(replaceRow, tryThis), puz.columnConstraints).solution
            case (solution, _) => solution
          }).map(transform)
        }
      }
    }
  }
}

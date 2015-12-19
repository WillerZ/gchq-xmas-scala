object Solve {

  import PuzzleBuilding._

  // @formatter:off
  val puzzle:Puzzle = """
                                    | | |1| | | | | | | | | | | | |2| | | | | |1| | | |
                                    | | |3|1|1| | | | |2| |1| | | |2| | | | | |3| | | |
                                    | | |1|3|3| |7| |2|2| |2| |3| |1|1| | | | |1| |1| |
                                    | |1|3|1|1|1|1| |1|1| |3| |3| |1|3| |7| |1|1|1|1|7|
                                    |7|1|1|1|1|1|1| |2|2|1|1|4|1|1|1|3| |1|1|3|1|3|2|1|
                                    |2|2|3|5|4|1|1| |1|1|7|1|1|1|2|1|2| |4|1|1|2|1|2|3|
                                    |1|2|1|1|1|2|1|1|8|1|3|1|1|1|5|1|1|6|1|1|3|1|4|2|2|
                                    |1|1|3|3|3|1|1|1|2|1|2|1|2|3|2|2|8|2|1|1|7|1|3|6|1|
                                    |7|1|1|1|1|1|7|3|1|2|1|1|6|1|2|1|1|1|3|4|1|4|3|1|1|""" vc
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
    puzzle.solution match {
      case None => println("No Solution")
      case Some(solution) => println(s"Solution is\n$solution")
    }
  }
}
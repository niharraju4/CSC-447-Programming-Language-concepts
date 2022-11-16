import org.scalatest.*
import org.scalatest.prop.TableDrivenPropertyChecks.*

class fp2tests extends UnitSpec:
  val EX : Map[Int, Tag] = 
    (for i <- (1 to 10).toList yield {
      object T extends Tag ("fp2ex%02d".format (i))
      (i, T)
    }).toMap

  import fp2.*

  property ("EX01 - map", EX (1)) {
    assert (map ((1 to 5).toList, (n:Int) => n + 1) === (2 to 6).toList)
    assert (map (Nil, (n:Int) => n + 1) === Nil)
  }

  property ("EX02 - filter", EX (2)) {
    assert (filter ((1 to 5).toList, (n:Int) => n % 2 === 1) === List (1,3,5))
    assert (filter (Nil, (n:Int) => false) === Nil)
    assert (filter (Nil, (n:Int) => true) === Nil)
    assert (filter ((1 to 5).toList, (n:Int) => false) === Nil)
    assert (filter ((1 to 5).toList, (n:Int) => true) === (1 to 5).toList)
  }

  property ("EX03 - append", EX (3)) {
    assert (append ((1 to 5).toList, (6 to 10).toList) === (1 to 10).toList)
    assert (append ((1 to 5).toList,              Nil) === (1 to 5).toList)
    assert (append (            Nil,              Nil) === Nil)
    assert (append (            Nil, (6 to 10).toList) === (6 to 10).toList)
  }

  property ("EX04 - flatten", EX (4)) {
    assert (flatten (List ((1 to 5).toList, (6 to 10).toList, (11 to 15).toList)) === (1 to 15).toList)
    assert (flatten (Nil) === Nil)
    assert (flatten (List ((1 to 5).toList)) === (1 to 5).toList)
    assert (flatten (List ((1 to 5).toList, (6 to 10).toList)) === (1 to 10).toList)
  }

  property ("EX05 - foldLeft", EX (5)) {
    def f (s : String, n : Int) : String = s + "[" + n + "]"
    assert (foldLeft ((1 to 3).toList, "@", f) === "@[1][2][3]")
    assert (foldLeft (Nil, "@", f) === "@")
  }

  property ("EX06 - foldRight", EX (6)) {
    def f (n : Int, s : String) : String = s + "[" + n + "]"
    assert (foldRight ((1 to 3).toList, "@", f) === "@[3][2][1]")
    assert (foldRight (Nil, "@", f) === "@")
  }

  property ("EX07 - joinTerminateRight", EX (7)) {
    assert (joinTerminateRight (List ("a","b","c","d"), ";") === "a;b;c;d;")
    assert (joinTerminateRight (Nil, ";") === "")
    assert (joinTerminateRight (List ("a"), ";") === "a;")
  }

  property ("EX08 - joinTerminateLeft", EX (8)) {
    assert (joinTerminateLeft (List ("a","b","c","d"), ";") === "a;b;c;d;")
    assert (joinTerminateLeft (Nil, ";") === "")
    assert (joinTerminateLeft (List ("a"), ";") === "a;")
  }

  property ("EX09 - firstNumGreaterOrEqual", EX (9)) {
    assert (firstNumGreaterOrEqual (5, List (4, 6, 8, 5)) === 6)
    assert (firstNumGreaterOrEqual (7, List (4, 6, 8, 5)) === 8)
    assert (firstNumGreaterOrEqual (4, List (4, 6, 8, 5)) === 4)
  }

  property ("EX10 - firstIndexNumGreaterOrEqual", EX (10)) {
    assert (firstIndexNumGreaterOrEqual (5, List (4, 6, 8, 5)) === 1)
    assert (firstIndexNumGreaterOrEqual (7, List (4, 6, 8, 5)) === 2)
    assert (firstIndexNumGreaterOrEqual (4, List (4, 6, 8, 5)) === 0)
  }


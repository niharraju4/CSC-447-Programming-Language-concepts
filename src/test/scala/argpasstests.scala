import org.scalatest.*
import org.scalatest.prop.TableDrivenPropertyChecks.*

class argpasstests extends UnitSpec:
  val EX : Map[Int, Tag] = 
    (for i <- (1 to 5).toList yield 
      object T extends Tag ("argpassex%02d".format (i))
      (i, T)
    ).toMap

  import argpass.*

  property ("EX01 - refint1", EX (1)) {
    val rand = scala.util.Random
    val x1 = rand.nextInt (100)
    val x2 = rand.nextInt (100)
    val x3 = rand.nextInt (100)
    val placeholder = RefInt(rand.nextInt (100))
    var r1 : RefInt = placeholder
    var state = 1
    val res = refint1 (
      r =>
        if r1 eq placeholder then
          r1 = r
        else
          if !(r1 eq r) then
            throw RuntimeException ("Only one instance of RefInt is allowed")
        state match 
          case 1 => r1 = r; r.set (x1); state = state + 1
          case 2 => r.set (x2); state = state + 1
          case 3 => r.set (x3); state = state + 1
          case _ => throw RuntimeException ("Called too many times")
    )
    assert (res === (x1, x2, x3))
  }

  property ("EX02 - refint2", EX (2)) {
    val rand = scala.util.Random
    val x1 = rand.nextInt (100)
    val x2 = rand.nextInt (100)
    val x3 = rand.nextInt (100)
    val placeholder = RefInt(rand.nextInt (100))
    var r1 : RefInt = placeholder
    var r2 : RefInt = placeholder
    var r3 : RefInt = placeholder
    var state = 1
    val res = refint2 (
      r =>
        for s <- List (r1, r2, r3) do
          if !(r eq placeholder) && (r eq s) then
            throw RuntimeException ("Must pass a new instance of RefInt for each call")
        state match
          case 1 => r1 = r; r.set (-2); state = state + 1
          case 2 => r2 = r; r.set (-2); state = state + 1
          case 3 => r3 = r; r1.set (x1); r2.set (x2); r3.set (x3); state = state + 1
          case _ => throw RuntimeException ("Called too many times")
    )
    assert (res === (x1, x2, x3))
  }

  property ("EX03 - refint3", EX (3)) {
    val rand = scala.util.Random
    val x1 = rand.nextInt (100)
    val r1 : RefInt = RefInt (x1)
    val r2 : RefInt = refint3 (r1)
    assert (r1.get() == x1 + 1)
    assert (r2.get() == x1 * 2)
  }

  property ("EX04 - refint4", EX (4)) {
    val rand = scala.util.Random
    val x1 = rand.nextInt (100)
    val r1 : RefInt = RefInt (x1)

    assert (refint4 (r1, (
      r2 =>
        if r1 eq r2 then throw RuntimeException ("Must use a copy of r, not r itself")
    )) == true)
                       
    assert (refint4 (r1, (
      r2 =>
        if r1 eq r2 then throw RuntimeException ("Must use a copy of r, not r itself")
        r2.set (r2.get() + 1)
    )) == false)
                       
  }

  property ("EX05 - refint5", EX (5)) {
    val rand = scala.util.Random
    val xs : List[Int] = (1 to 5).toList.map (x => rand.nextInt (100))
    val ys : List[RefInt] = xs.map (x => RefInt (x))
    refint5 (ys)
    var changed = false
    for (x, r) <- xs.zip (ys) do 
      if x != r.get() then 
        changed = true
    if !changed then
      throw RuntimeException ("xs is unchanged!")
  }


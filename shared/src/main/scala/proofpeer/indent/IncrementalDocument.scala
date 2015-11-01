package proofpeer.indent

trait IncrementalDocument extends Document {
  def appendNewlines(num : Int) : IncrementalDocument
  def appendSpaces(num : Int) : IncrementalDocument
  def appendCharacters(characters : Vector[Int], from : Int, len : Int) : IncrementalDocument 
  def cutoffAfter(position : Int) : IncrementalDocument
  def cutoffBefore(position : Int) : IncrementalDocument
}

final class IncDoc(characters : Vector[(Int, Int, Int)], rows : Vector[Int], currentRow : Int, currentColumn : Int) 
  extends IncrementalDocument 
{

  override def toString : String = 
    "IncDoc(characters="+characters+", rows="+rows+", currentRow="+currentRow+", currentColumn"+currentColumn+")"

  def size : Int = characters.size

  def firstPositionInRow(row : Int) = rows(row)

  def character(position : Int) : (Int, Int, Int) = characters(position)

  def appendNewlines(num : Int) : IncrementalDocument = {
    if (num <= 0) this
    else new IncDoc(characters, rows, currentRow + num, 0)
  }

  def appendSpaces(num : Int) : IncrementalDocument = {
    if (num <= 0) this
    else new IncDoc(characters, rows, currentRow, currentColumn + num)
  }

  def appendCharacters(chars : Vector[Int], from : Int, len : Int) : IncrementalDocument = {
    var _characters = characters 
    var column = currentColumn
    var i = from
    val to = from + len
    while (i < to) {
      val char = chars(i)
      _characters = _characters :+ (currentRow, column, char)
      column = column + 1
      i = i + 1
    }
    new IncDoc(_characters, rows ++ Vector.fill(currentRow + 1 - rows.size)(size), currentRow, column)
  }

  def cutoffAfter(position : Int) : IncrementalDocument = {
    val (row, column, _) = characters(position)
    new IncDoc(characters.take(position + 1), rows.take(row + 1), row, column + 1)
  }

  def cutoffBefore(position : Int) : IncrementalDocument = {    
    val (row, column, _) = characters(position)
    if (position == 0)
      new IncDoc(Vector(), Vector(), row, column)
    else {
      val (rowBefore, _, _) = characters(position - 1)
      new IncDoc(characters.take(position), rows.take(rowBefore + 1), row, column)
    }
  }

} 

final object IncrementalDocument {

  val empty : IncrementalDocument = new IncDoc(Vector(), Vector(), 0, 0)

}


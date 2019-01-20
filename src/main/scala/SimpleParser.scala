//A PARSER IS A COMBINATION OF "STACK" "SYMBOL TABLE" AND "OPERATION STRUCTURE"
//THIS PROGRAM IS A HOMEWORK OF USTC PARSER CLASS
//USING SIMPLEST CONCEPT
//STACK PARSER
class SimpleParser(val func : String) {
  val token = new Token(func)
  val OperatorStack : Stack[Int] = new Stack[Int]
  var flag : Int = 0
  def parse() : Unit = {
    for (tok <- token) {
      tok match {
        case "+" =>
          valid(tok)
          add()
        case _ =>
          valid(tok)
          push(Integer.valueOf(tok))
      }
    }
    add()
    println(OperatorStack.pop)
  }

  def add() : Unit = {
    var sum = 0
    while (OperatorStack.hasNext) {
      val i = OperatorStack.pop
      sum += i
    }
    OperatorStack.push(sum)
  }

  def valid(str : String) : Unit = {
    // TODO TBC
//    assert(flag != 0 && str != "+") ; flag = 0
//    assert(flag == 0 && str == "+") ; flag = 1
  }
  def push(i : Int) : Unit = {
    OperatorStack.push(i)
  }
}
object SimpleParser {
  def main(args: Array[String]): Unit = {
    new SimpleParser("1 + 2 + 3").parse()
  }
}
class Token (func : String) extends Iterator[String] {
  private val tokens : Array[String] = func.split(" ")
  private var index : Int = 0
  def next : String = {
    val str = tokens.apply(index)
    index += 1
    str
  }
  def hasNext : Boolean = {
    index < tokens.length
  }
}

class Stack[T]() extends Iterator[T]{
  var stackBase : List[T] = List()

  def pop : T = {
    next()
  }

  def push(t : T) : Unit = {
    this.stackBase = t :: stackBase
  }

  override def hasNext: Boolean = stackBase.nonEmpty

  override def next(): T = {
    val popVal = stackBase.head
    stackBase = stackBase.tail
    popVal
  }
}

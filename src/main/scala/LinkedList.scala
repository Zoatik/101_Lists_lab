case class Node(var item: String, var next: Node = null){}

class LinkedList(var head: Node = null){

  override def toString(): String = {
    var text = s"List content (size ${getSize()}) : "
    var currNode = head
    while (currNode != null){
      text += s"${currNode.item} -> "
      currNode = currNode.next
    }
    text += "null"
    text
  }

  def addToStart(s:String): Unit = {
    val new_node = Node(s, head)
    head = new_node
  }

  def getSize(): Int = {
    var counter = 0
    var currNode = head
    while (currNode != null){
      currNode = currNode.next
      counter += 1
    }
    counter
  }

  def removeFirstElement(): Unit = {
    if (head != null)
      head = head.next
  }

  def getLastElement(): Node = {
    var currNode = head
    while (currNode != null){
      if (currNode.next == null)
        return currNode
      currNode = currNode.next
    }
    null
  }

  def addToEnd(item: String): Unit = {
    val lastElement = getLastElement()
    if (lastElement == null){
      head = Node(item)
    }
    else getLastElement().next = Node(item)
  }

  def isPresent(item: String): Boolean = {
    var currNode = head
    while(currNode != null){
      if (currNode.item == item)
        return true
      currNode = currNode.next
    }
    false
  }

  def findElement(item: String): Node = {
    var currNode = head
    while(currNode != null){
      if (currNode.item == item)
        return currNode
      currNode = currNode.next
    }
    null
  }

  def swapElements(item1: String, item2: String): Unit = {
    val node1 = findElement(item1)
    val node2 = findElement(item2)
    if (node1 != null && node2 != null){
      node1.item = item2
      node2.item = item1
    }
  }

  def removeLastElement(): Unit = {
    var currNode = head

    if (currNode == null)
      return
    if (currNode.next == null){
      head = null
      return
    }
    while(currNode.next != null){
      if (currNode.next.next == null) {
        currNode.next = null
        return
      }
      currNode = currNode.next
    }
  }

  def removeElement(item: String): Unit = {
    var currNode = head
    if (currNode == null) {
      println("CurrNode is null")
      return
    }
    if (currNode.next == null){
      head = null
      return
    }
    if (currNode.item == item){
      head = currNode.next
      return
    }
    println("Currnode : " + currNode.toString)
    while(currNode.next != null){
      if (currNode.next.item == item){
        currNode.next = currNode.next.next
      }
      else
        currNode = currNode.next
    }
  }

  def insertAfter(before: String, after: String): Unit = {
    val nodeBefore = findElement(before)
    if (nodeBefore != null){
      val newNode = Node(after, nodeBefore.next)
      nodeBefore.next = newNode
    }
  }

}

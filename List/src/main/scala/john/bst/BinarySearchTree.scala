package john.bst

abstract sealed class BinarySearchTree[+A <% Ordered[A]] {
  def value: A
  def left: BinarySearchTree[A]
  def right: BinarySearchTree[A]
  def size: Int
  def isEmpty: Boolean
  
  def isValid: Boolean = {
    if (isEmpty) true
    else if (left.isEmpty && right.isEmpty) true
    else if (left.isEmpty) right.value >= value && right.isValid
    else if (right.isEmpty) left.value <= value && left.isValid
    else left.value <= value && right.value >= value && left.isValid && right.isValid
  }
  
  def isBalanced: Boolean = {
    
  }
} 
